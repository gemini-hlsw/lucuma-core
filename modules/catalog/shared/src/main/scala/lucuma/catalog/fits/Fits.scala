// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.syntax.all.*
import fs2.Chunk
import fs2.Pipe
import fs2.Pull
import fs2.RaiseThrowable
import fs2.Stream

import java.nio.ByteBuffer

/**
 * Streaming reader for FITS files.
 *
 * This reader understands header data units and binary table extensions whose columns are of type
 * `nA`, `1J`, `1E` or `1D`. It does not decode image data, compressed extensions, `TDIM`, variable
 * length arrays or ASCII tables.
 *
 * Header data units are traversed lazily and image data is skipped rather than read, so [[headers]]
 * and [[binaryTable]] terminate as soon as they have what they need. That matters for files where
 * the data dwarfs the metadata, such as pre images.
 */
object Fits:

  /** Every FITS file is a whole number of these. */
  val BlockSize: Int = 2880

  /** Every header record is exactly this long. */
  val CardSize: Int = 80

  val CardsPerBlock: Int = BlockSize / CardSize

  private val EndKeyword = "END"

  /**
   * Emits the header of each header data unit in turn, skipping over data sections.
   *
   * Take from the resulting stream to stop early: nothing beyond the headers consumed is read.
   */
  def headers[F[_]: RaiseThrowable]: Pipe[F, Byte, FitsHeader] = in =>
    walkHdus(in, Pull.done)((h, _) => Right(Pull.output1(h))).stream

  /**
   * Emits the structure of the first binary table extension, then terminates.
   *
   * The rows are not read, so this is cheap even on a large file.
   */
  def binaryTable[F[_]: RaiseThrowable]: Pipe[F, Byte, FitsBinaryTable] = in =>
    walkHdus(in, Pull.raiseError(FitsProblem.NoBinaryTable))((h, _) =>
      if h.isBinaryTable then
        Left(FitsBinaryTable.fromHeader(h).fold(Pull.raiseError, Pull.output1))
      else Right(Pull.pure(()))
    ).stream

  /**
   * Emits every row of the first binary table extension.
   *
   * The first extension in '''forward''' order is used, and `EXTNAME` is ignored: requiring a
   * particular extension name would reject files that are structurally fine.
   */
  def binaryTableRows[F[_]: RaiseThrowable]: Pipe[F, Byte, FitsRow] = in =>
    walkHdus(in, Pull.raiseError(FitsProblem.NoBinaryTable))((h, rest) =>
      if h.isBinaryTable then
        Left(FitsBinaryTable.fromHeader(h).fold(Pull.raiseError, t => readRows(rest, t, 0L)))
      else Right(Pull.pure(()))
    ).stream

  /**
   * Walks the header data units in forward order, raising `NotFitsFormat` if the first is not a
   * primary header.
   *
   * `onHdu` sees each header together with the stream positioned just after it. A `Left` is
   * terminal; a `Right` runs and the walk skips the unit's data section and moves to the next
   * header. `onEnd` runs at a clean end of stream.
   */
  private def walkHdus[F[_]: RaiseThrowable, O](
    in:    Stream[F, Byte],
    onEnd: Pull[F, O, Unit]
  )(
    onHdu: (FitsHeader, Stream[F, Byte]) => Either[Pull[F, O, Unit], Pull[F, O, Unit]]
  ): Pull[F, O, Unit] =
    def go(s: Stream[F, Byte], primary: Boolean): Pull[F, O, Unit] =
      readHeader(s, Nil).flatMap:
        case None            => onEnd
        case Some((h, rest)) =>
          if primary && !h.isPrimary then Pull.raiseError(FitsProblem.NotFitsFormat)
          else
            onHdu(h, rest) match
              case Left(terminal) => terminal
              case Right(prefix)  => prefix >> go(skipData(rest, h), false)

    go(in, true)

  /**
   * Reads header blocks until the END card.
   *
   * Yields `None` at a clean end of stream, which is how the traversal knows it has run out of
   * header data units rather than hit a malformed one.
   */
  private def readHeader[F[_]: RaiseThrowable](
    s:   Stream[F, Byte],
    acc: List[FitsHeader.Card]
  ): Pull[F, Nothing, Option[(FitsHeader, Stream[F, Byte])]] =
    s.pull
      .unconsN(BlockSize, allowFewer = true)
      .flatMap:
        case None                                       =>
          if acc.isEmpty then Pull.pure(None)
          else Pull.raiseError(FitsProblem.UnterminatedHeader)
        case Some((chunk, _)) if chunk.size < BlockSize =>
          Pull.raiseError(FitsProblem.IncompleteBlock(chunk.size.toLong))
        case Some((chunk, rest))                        =>
          val records = decodeRecords(chunk)
          val endIdx  = records.indexWhere(_.take(8).trim === EndKeyword)
          val cards   = (if endIdx < 0 then records else records.take(endIdx))
            .flatMap(FitsHeader.parseCard)
            .toList
          if endIdx < 0 then readHeader(rest, acc ::: cards)
          else Pull.pure((FitsHeader(acc ::: cards), rest).some)

  /**
   * Emits exactly the declared number of rows.
   */
  private def readRows[F[_]: RaiseThrowable](
    s:     Stream[F, Byte],
    table: FitsBinaryTable,
    read:  Long
  ): Pull[F, FitsRow, Unit] =
    if read >= table.rowCount then Pull.done
    else
      s.pull
        .unconsN(table.rowLength, allowFewer = true)
        .flatMap:
          case None                                             =>
            Pull.raiseError(FitsProblem.TruncatedData(table.rowCount, read))
          case Some((chunk, _)) if chunk.size < table.rowLength =>
            Pull.raiseError(FitsProblem.TruncatedData(table.rowCount, read))
          case Some((chunk, rest))                              =>
            Pull.output1(decodeRow(chunk, table)) >> readRows(rest, table, read + 1)

  /** Skips a unit's data section, including its padding to a block boundary. */
  private def skipData[F[_]](s: Stream[F, Byte], header: FitsHeader): Stream[F, Byte] =
    val size   = header.dataSize
    val padded = if size % BlockSize === 0L then size else (size / BlockSize + 1) * BlockSize
    s.drop(padded)

  /** Splits a header block into its fixed width records. FITS headers are ASCII by definition. */
  private def decodeRecords(chunk: Chunk[Byte]): Vector[String] =
    val arr = chunk.toArray
    Vector.tabulate(CardsPerBlock): i =>
      val sb = new StringBuilder(CardSize)
      var j  = 0
      while j < CardSize do
        sb.append((arr(i * CardSize + j) & 0xff).toChar)
        j += 1
      sb.result()

  /** Decodes one fixed stride row. FITS numeric data is big endian, as ByteBuffer defaults to. */
  private def decodeRow(chunk: Chunk[Byte], table: FitsBinaryTable): FitsRow =
    val arr   = chunk.toArray
    val bb    = ByteBuffer.wrap(arr)
    val cells = table.columns.map: c =>
      c.format match
        case FitsColumnFormat.Int32   => FitsCell.IntCell(bb.getInt(c.byteOffset).toLong)
        case FitsColumnFormat.Float32 => FitsCell.RealCell(bb.getFloat(c.byteOffset).toDouble)
        case FitsColumnFormat.Float64 => FitsCell.RealCell(bb.getDouble(c.byteOffset))
        case FitsColumnFormat.Text    =>
          val sb = new StringBuilder(c.repeat)
          var j  = 0
          while j < c.repeat do
            sb.append((arr(c.byteOffset + j) & 0xff).toChar)
            j += 1
          FitsCell.TextCell(sb.result())
    FitsRow(table, cells)

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
 * '''Scope.''' This reader understands header data units and binary table extensions whose columns
 * are of type `nA`, `1J`, `1E` or `1D`. It does not decode image data, compressed extensions,
 * `TDIM`, variable length arrays or ASCII tables. Widening that set changes the module's public
 * contract, so it is stated here rather than left to be discovered.
 *
 * Header data units are traversed lazily and image data is skipped rather than read, so
 * [[headers]] and [[binaryTable]] terminate as soon as they have what they need. That matters for
 * files where the data dwarfs the metadata, such as pre images.
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
    def go(s: Stream[F, Byte], primary: Boolean): Pull[F, FitsHeader, Unit] =
      readHeader(s, Nil).flatMap:
        case None            => Pull.done
        case Some((h, rest)) =>
          if primary && !h.isPrimary then Pull.raiseError(FitsProblem.NotFitsFormat)
          else Pull.output1(h) >> go(skipData(rest, h), false)

    go(in, true).stream

  /**
   * Emits the structure of the first binary table extension, then terminates.
   *
   * The rows are not read, so this is cheap even on a large file.
   */
  def binaryTable[F[_]: RaiseThrowable]: Pipe[F, Byte, FitsBinaryTable] = in =>
    def go(s: Stream[F, Byte], primary: Boolean): Pull[F, FitsBinaryTable, Unit] =
      readHeader(s, Nil).flatMap:
        case None            => Pull.raiseError(FitsProblem.NoBinaryTable)
        case Some((h, rest)) =>
          if primary && !h.isPrimary then Pull.raiseError(FitsProblem.NotFitsFormat)
          else if h.isBinaryTable then
            FitsBinaryTable.fromHeader(h).fold(Pull.raiseError, Pull.output1)
          else go(skipData(rest, h), false)

    go(in, true).stream

  /**
   * Emits every row of the first binary table extension.
   *
   * The first extension in '''forward''' order is used, and `EXTNAME` is ignored: requiring a
   * particular extension name would reject files that are structurally fine.
   */
  def binaryTableRows[F[_]: RaiseThrowable]: Pipe[F, Byte, FitsRow] = in =>
    def go(s: Stream[F, Byte], primary: Boolean): Pull[F, FitsRow, Unit] =
      readHeader(s, Nil).flatMap:
        case None            => Pull.raiseError(FitsProblem.NoBinaryTable)
        case Some((h, rest)) =>
          if primary && !h.isPrimary then Pull.raiseError(FitsProblem.NotFitsFormat)
          else if h.isBinaryTable then
            FitsBinaryTable.fromHeader(h) match
              case Left(p)      => Pull.raiseError(p)
              case Right(table) => readRows(rest, table, 0L)
          else go(skipData(rest, h), false)

    go(in, true).stream

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
    s.pull.unconsN(BlockSize, allowFewer = true).flatMap:
      case None                                          =>
        if acc.isEmpty then Pull.pure(None)
        else Pull.raiseError(FitsProblem.UnterminatedHeader)
      case Some((chunk, _)) if chunk.size < BlockSize    =>
        Pull.raiseError(FitsProblem.IncompleteBlock(chunk.size.toLong))
      case Some((chunk, rest))                           =>
        val records = decodeRecords(chunk)
        val endIdx  = records.indexWhere(_.take(8).trim === EndKeyword)
        val cards   = (if endIdx < 0 then records else records.take(endIdx))
                        .flatMap(FitsHeader.parseCard)
                        .toList
        if endIdx < 0 then readHeader(rest, acc ::: cards)
        else Pull.pure(Some((FitsHeader(acc ::: cards), rest)))

  /**
   * Emits exactly the declared number of rows.
   *
   * Stopping at `rowCount` rather than at the end of the data section is essential: the section is
   * zero padded up to a block boundary, and the row stride does not divide the block size, so the
   * padding would otherwise decode as extra rows. Reading row by row also lets truncation be
   * reported precisely, which chunking cannot do.
   */
  private def readRows[F[_]: RaiseThrowable](
    s:     Stream[F, Byte],
    table: FitsBinaryTable,
    read:  Long
  ): Pull[F, FitsRow, Unit] =
    if read >= table.rowCount then Pull.done
    else
      s.pull.unconsN(table.rowLength, allowFewer = true).flatMap:
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
    val arr = chunk.toArray
    val bb  = ByteBuffer.wrap(arr)
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

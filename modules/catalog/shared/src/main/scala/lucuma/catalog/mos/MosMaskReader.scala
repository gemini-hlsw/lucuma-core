// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.syntax.all.*
import fs2.Pipe
import fs2.Pull
import fs2.RaiseThrowable
import fs2.Stream
import lucuma.catalog.fits.Fits
import lucuma.catalog.fits.FitsBinaryTable
import lucuma.catalog.fits.FitsProblem
import lucuma.catalog.fits.FitsRow
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.enums.MosSlitType
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskSlit
import lucuma.core.model.mos.MosObjectId
import lucuma.core.model.mos.MosSpectrumFootprint
import lucuma.core.util.Enumerated

/**
 * Reads Gemini MOS mask designs from FITS.
 *
 * The same design is called an object definition file when a principal investigator submits it and
 * a mask definition file once converted for the mask cutter. They share a schema and both are read
 * here; converting between them is out of scope.
 *
 * Failures are raised into the stream rather than reported per row. A FITS binary table has a fixed
 * row stride, so a row that fails to decode means every following offset is wrong — there is no
 * such thing as skipping one bad row and carrying on.
 */
object MosMaskReader:

  /**
   * Emits the design's metadata, then terminates.
   *
   * The slit rows are not read, so this is cheap regardless of how many slits the design places.
   */
  def header[F[_]: RaiseThrowable]: Pipe[F, Byte, MosMaskHeader] = in =>
    asMaskProblem:
      in.through(Fits.binaryTable)
        .flatMap(t => emitOrRaise(MosMaskHeaderDecoder.decode(t.header)))

  /**
   * Emits every slit in the design.
   *
   * Where the design's metadata is also wanted, run [[header]] over the same source — it stops
   * after the header rather than reading the rows.
   */
  def slits[F[_]: RaiseThrowable]: Pipe[F, Byte, MosMaskSlit] = in =>
    asMaskProblem:
      in.through(Fits.binaryTableRows)
        .zipWithIndex
        .pull
        .uncons1
        .flatMap {
          // A table with no rows is structurally valid: a design that placed no slits.
          case None                     => Pull.done
          case Some(((first, _), rest)) =>
            (MosMaskHeaderDecoder.decode(first.table.header),
             MosMaskColumns.resolve(first.table)
            ).tupled match
              case Left(problem)    => Pull.raiseError(problem)
              case Right((h, cols)) =>
                (Stream.emit((first, 0L)) ++ rest)
                  .flatMap((row, index) => emitOrRaise(decodeSlit(row, index, h, cols)))
                  .pull
                  .echo
        }
        .stream

  private def emitOrRaise[F[_]: RaiseThrowable, A](e: Either[MosMaskProblem, A]): Stream[F, A] =
    e.fold(Stream.raiseError[F](_), Stream.emit)

  /**
   * Restates a FITS level failure as a mask level one.
   *
   * The two layers have separate problem types because the FITS reader is usable on its own, but a
   * caller of this reader should not have to catch both. Anything that is not a `FitsProblem` — an
   * IO failure from the source, say — passes through untouched.
   */
  private def asMaskProblem[F[_]: RaiseThrowable, A](s: Stream[F, A]): Stream[F, A] =
    s.handleErrorWith:
      case p: FitsProblem => Stream.raiseError(MosMaskProblem.Fits(p))
      case t              => Stream.raiseError(t)

  /** Builds one slit, resolving the file's x and y columns into physical width and length. */
  private def decodeSlit(
    row:    FitsRow,
    index:  Long,
    header: MosMaskHeader,
    cols:   MosMaskColumns
  ): Either[MosMaskProblem, MosMaskSlit] =
    def real(idx: Int, name: String): Either[MosMaskProblem, Double] =
      row(idx).flatMap(_.asDouble).toRight(MosMaskProblem.MissingCell(index, name))

    def optReal(idx: Option[Int], name: String): Either[MosMaskProblem, Option[Double]] =
      idx.traverse(i => real(i, name))

    for
      idValue  <- row(cols.id).flatMap(_.asInt).toRight(MosMaskProblem.MissingCell(index, "ID"))
      raHours  <- real(cols.ra, "RA")
      decDeg   <- real(cols.dec, "DEC")
      dec      <- Declination
                    .fromDoubleDegrees(decDeg)
                    .toRight(MosMaskProblem.InvalidCell(index, "DEC", decDeg.toString))
      x        <- real(cols.x, "x_ccd")
      y        <- real(cols.y, "y_ccd")
      mag      <- real(cols.mag, "MAG").flatMap: d =>
                    BrightnessValue
                      .from(BigDecimal(d))
                      .leftMap(_ => MosMaskProblem.InvalidCell(index, "MAG", d.toString))
      posX     <- real(cols.slitPosX, "slitpos_x")
      posY     <- real(cols.slitPosY, "slitpos_y")
      sizeX    <- real(cols.slitSizeX, "slitsize_x")
      sizeY    <- real(cols.slitSizeY, "slitsize_y")
      tilt     <- real(cols.slitTilt, "slittilt")
      priority <- charCell(row, cols.priority, index, "priority")
                    .flatMap: c =>
                      Enumerated[MosSlitPriority]
                        .fromTag(c.toString)
                        .toRight(MosMaskProblem.InvalidCell(index, "priority", c.toString))
      slitType <- charCell(row, cols.slitType, index, "slittype")
                    .flatMap: c =>
                      Enumerated[MosSlitType]
                        .fromTag(c.toString)
                        .toRight(MosMaskProblem.InvalidCell(index, "slittype", c.toString))
      redshift <- optReal(cols.redshift, "redshift")
      left     <- optReal(cols.specLeft, "specleft")
      right    <- optReal(cols.specRight, "specright")
      bottom   <- optReal(cols.specBottom, "specbottom")
      top      <- optReal(cols.specTop, "spectop")
    yield
      val horizontal = header.dispersionDirection === MosDispersionDirection.Horizontal
      // Width is always the extent along the dispersion direction.
      val width      = if horizontal then sizeX else sizeY
      val length     = if horizontal then sizeY else sizeX
      // Likewise, an offset "along the slit" runs along its length.
      val along      = if horizontal then posY else posX
      val across     = if horizontal then posX else posY

      MosMaskSlit(
        id = MosObjectId(idValue),
        // The RA column is in hours, unlike the header's pointing keywords, which are degrees.
        coordinates = Coordinates(RightAscension.fromDoubleDegrees(raHours * 15.0), dec),
        x = x,
        y = y,
        magnitude = mag,
        slitWidth = Angle.fromDoubleArcseconds(width),
        slitLength = Angle.fromDoubleArcseconds(length),
        offsetAlongSlit = Angle.fromDoubleArcseconds(along),
        offsetAcrossSlit = Angle.fromDoubleArcseconds(across),
        tilt = Angle.fromDoubleDegrees(tilt),
        slitType = slitType,
        priority = priority,
        // Zero is retained rather than read as "absent". Mask files use it for both an unknown
        // redshift and a genuine zero, and inventing the distinction would lose information.
        redshift = redshift.map(z => Redshift(BigDecimal(z))),
        spectrumFootprint = (left, right, bottom, top).tupled.map(MosSpectrumFootprint.apply)
      )

  private def charCell(
    row:   FitsRow,
    idx:   Int,
    index: Long,
    name:  String
  ): Either[MosMaskProblem, Char] =
    row(idx).flatMap(_.asChar).toRight(MosMaskProblem.MissingCell(index, name))

/**
 * Positions of a mask design's columns within its table, resolved by name once — mask design
 * software writes columns in varying order, so positions cannot be assumed.
 *
 * The required set is drawn where the format's own history drew it. Object identity, position,
 * brightness and slit geometry have always been present; `redshift` and the spectrum footprint were
 * added to the format later, so files legitimately lack them and they stay optional.
 */
private case class MosMaskColumns(
  id:         Int,
  ra:         Int,
  dec:        Int,
  x:          Int,
  y:          Int,
  mag:        Int,
  slitPosX:   Int,
  slitPosY:   Int,
  slitSizeX:  Int,
  slitSizeY:  Int,
  slitTilt:   Int,
  priority:   Int,
  slitType:   Int,
  redshift:   Option[Int],
  specLeft:   Option[Int],
  specRight:  Option[Int],
  specBottom: Option[Int],
  specTop:    Option[Int]
)

private object MosMaskColumns:

  def resolve(table: FitsBinaryTable): Either[MosMaskProblem, MosMaskColumns] =
    def required(name: String) = table.indexOf(name).toValidNel(name)
    def optional(name: String) = table.indexOf(name)

    (
      required("ID"),
      required("RA"),
      required("DEC"),
      required("x_ccd"),
      required("y_ccd"),
      required("MAG"),
      required("slitpos_x"),
      required("slitpos_y"),
      required("slitsize_x"),
      required("slitsize_y"),
      required("slittilt"),
      required("priority"),
      required("slittype")
    ).mapN: (id, ra, dec, x, y, mag, px, py, sx, sy, tilt, prio, stype) =>
      MosMaskColumns(
        id,
        ra,
        dec,
        x,
        y,
        mag,
        px,
        py,
        sx,
        sy,
        tilt,
        prio,
        stype,
        optional("redshift"),
        optional("specleft"),
        optional("specright"),
        optional("specbottom"),
        optional("spectop")
      )
    .toEither
      .leftMap(MosMaskProblem.MissingColumns.apply)

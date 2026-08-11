// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.syntax.all.*
import lucuma.catalog.fits.FitsHeader
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskProvenance
import lucuma.core.model.mos.MosNodAndShuffle
import lucuma.core.model.mos.MosSpectroscopyConfig
import lucuma.core.util.Timestamp

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

/** Interprets the keywords of a mask file's binary table extension. */
object MosMaskHeaderDecoder:

  /**
   * The three instruments that do multi-object spectroscopy at Gemini, keyed by the value their
   * mask files carry in `INSTRUME`.
   *
   * Not derived from `Instrument.shortName`: that matches for the GMOS arms but not for
   * Flamingos-2, whose files say `F2`.
   */
  private val Instruments: Map[String, Instrument] =
    Map(
      "GMOS-N" -> Instrument.GmosNorth,
      "GMOS-S" -> Instrument.GmosSouth,
      "F2"     -> Instrument.Flamingos2
    )

  /** Dispersion direction implied by the instrument, for files predating the `DISPDIR` keyword. */
  private def impliedDirection(i: Instrument): MosDispersionDirection =
    i match
      case Instrument.Flamingos2 => MosDispersionDirection.Vertical
      case _                     => MosDispersionDirection.Horizontal

  def decode(h: FitsHeader): Either[MosMaskProblem, MosMaskHeader] =
    for
      instrument <- decodeInstrument(h)
      pixelScale <- h.double("PIXSCALE").toRight(MosMaskProblem.MissingKeyword("PIXSCALE"))
      pointing   <- decodePointing(h)
    yield MosMaskHeader(
      instrument = instrument,
      dispersionDirection = h
        .string("DISPDIR")
        .map(_.trim)
        .flatMap(MosDispersionDirection.fromFitsValue.getOption)
        .getOrElse(impliedDirection(instrument)),
      pixelScale = pixelScale,
      pointing = pointing,
      positionAngle = h.double("MASK_PA").map(Angle.fromDoubleDegrees),
      hasTiltedSlits = h.int("TILTSLIT").exists(_ =!= 0),
      nodAndShuffle = decodeNodAndShuffle(h),
      spectroscopy = decodeSpectroscopy(h),
      provenance = decodeProvenance(h),
      keywords = h.rawValues
    )

  private def decodeInstrument(h: FitsHeader): Either[MosMaskProblem, Instrument] =
    h.string("INSTRUME").map(_.trim) match
      case None    => MosMaskProblem.MissingKeyword("INSTRUME").asLeft
      case Some(s) =>
        Instruments.get(s).toRight(MosMaskProblem.InvalidKeyword("INSTRUME", s))

  /**
   * Pointing centre, in degrees.
   *
   * `RA_IMAG` and `DEC_IMAG` were called `RA` and `DEC` in early versions of the format, so both
   * spellings are accepted. Note that these are degrees, unlike the table's `RA` column, which is
   * hours.
   */
  private def decodePointing(h: FitsHeader): Either[MosMaskProblem, Coordinates] =
    val ra  = h.double("RA_IMAG").orElse(h.double("RA"))
    val dec = h.double("DEC_IMAG").orElse(h.double("DEC"))
    (ra, dec) match
      case (None, _)          => MosMaskProblem.MissingKeyword("RA_IMAG").asLeft
      case (_, None)          => MosMaskProblem.MissingKeyword("DEC_IMAG").asLeft
      case (Some(r), Some(d)) =>
        Declination
          .fromDoubleDegrees(d)
          .toRight(MosMaskProblem.InvalidKeyword("DEC_IMAG", d.toString))
          .map(dd => Coordinates(RightAscension.fromDoubleDegrees(r), dd))

  /**
   * Nod & Shuffle configuration.
   *
   * The keywords appear in fixed groups, so an unrecognised or absent `SHUFMODE` means the design
   * is simply not a Nod & Shuffle one.
   */
  private def decodeNodAndShuffle(h: FitsHeader): MosNodAndShuffle =
    val shuffle = h.int("SHUFSIZE").getOrElse(0)
    val binning = h.int("BINNING").getOrElse(1)
    h.string("SHUFMODE").map(_.trim) match
      case Some("microShuffle") =>
        MosNodAndShuffle.MicroShuffle(
          shuffle,
          binning,
          Angle.fromDoubleArcseconds(h.double("SLITLEN").getOrElse(0.0))
        )
      case Some("bandShuffle")  =>
        MosNodAndShuffle.BandShuffle(
          shuffle,
          binning,
          h.int("BANDSIZE").getOrElse(0),
          h.int("YOFFSET").getOrElse(0)
        )
      case _                    => MosNodAndShuffle.None

  private def decodeSpectroscopy(h: FitsHeader): MosSpectroscopyConfig =
    def nm(keyword: String): Option[Wavelength] =
      h.double(keyword)
        .filter(_ > 0)
        .flatMap(d => Wavelength.decimalNanometers.getOption(BigDecimal(d)))

    MosSpectroscopyConfig(
      filter = h.string("FILTSPEC").map(_.trim).filter(_.nonEmpty),
      grating = h.string("GRATING").map(_.trim).filter(_.nonEmpty),
      centralWavelength = nm("WAVELENG"),
      minWavelength = nm("SPEC_MIN"),
      maxWavelength = nm("SPEC_MAX"),
      dispersion = h.double("SPEC_DIS"),
      spectrumLength = h.double("SPEC_LEN"),
      anamorphicFactor = h.double("ANAMORPH")
    )

  private def decodeProvenance(h: FitsHeader): MosMaskProvenance =
    MosMaskProvenance(
      softwareVersion = h.string("GMMPSVER").map(_.trim).filter(_.nonEmpty),
      designer = h.string("PERS_ODF").map(_.trim).filter(_.nonEmpty),
      designedAt = h.string("DATE_ODF").flatMap(parseTimestamp),
      sourceObjectTable = h.string("FILE_OT").map(_.trim).filter(_.nonEmpty),
      detectorIdImaging = h.string("DET_IMG").map(_.trim).filter(_.nonEmpty),
      detectorIdSpectroscopy = h.string("DET_SPEC").map(_.trim).filter(_.nonEmpty)
    )

  /** `DATE_ODF` is an ISO-8601 local date and time, without a zone. */
  private def parseTimestamp(s: String): Option[Timestamp] =
    try Timestamp.fromLocalDateTime(LocalDateTime.parse(s.trim, DateTimeFormatter.ISO_DATE_TIME))
    catch case NonFatal(_) => none

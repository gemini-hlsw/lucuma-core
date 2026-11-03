// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.syntax.all.*
import coulomb.syntax.*
import lucuma.catalog.fits.FitsHeader
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.math.units.ArcSecondPerPixel
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.math.units.Pixels
import lucuma.core.model.mos.MosMaskHeader
import lucuma.core.model.mos.MosMaskProvenance
import lucuma.core.model.mos.MosNodAndShuffle
import lucuma.core.model.mos.MosSpectroscopyConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

/** Interprets the keywords of a mask file's binary table extension. */
object MosMaskHeaderDecoder:

  extension (h: FitsHeader)
    /** A string keyword, with whitespace-only values read as absent. */
    private def nonBlank(keyword: String): Option[String] =
      h.string(keyword).map(_.trim).filter(_.nonEmpty)

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
        .nonBlank("DISPDIR")
        .flatMap(Enumerated[MosDispersionDirection].fromTag)
        .getOrElse(impliedDirection(instrument)),
      pixelScale = BigDecimal(pixelScale).withUnit[ArcSecondPerPixel],
      pointing = pointing,
      positionAngle = h.double("MASK_PA").map(Angle.fromDoubleDegrees),
      hasTiltedSlits = h.int("TILTSLIT").exists(_ =!= 0),
      nodAndShuffle = decodeNodAndShuffle(h),
      spectroscopy = decodeSpectroscopy(h),
      provenance = decodeProvenance(h),
      keywords = h.rawValues
    )

  private def decodeInstrument(h: FitsHeader): Either[MosMaskProblem, Instrument] =
    h.nonBlank("INSTRUME") match
      case None    => MosMaskProblem.MissingKeyword("INSTRUME").asLeft
      case Some(s) =>
        Instruments.get(s).toRight(MosMaskProblem.InvalidKeyword("INSTRUME", s))

  /**
   * Pointing centre.
   *
   * `RA_IMAG` and `DEC_IMAG` were called `RA` and `DEC` in early versions of the format, so both
   * spellings are accepted. Values are either decimal degrees or quoted sexagesimal strings; GMMPS
   * writes both. Note that the degrees form differs from the table's `RA` column, which is hours.
   */
  private def decodePointing(h: FitsHeader): Either[MosMaskProblem, Coordinates] =
    def keyword(primary: String, alternate: String): Either[MosMaskProblem, (String, String)] =
      h.raw(primary)
        .tupleLeft(primary)
        .orElse(h.raw(alternate).tupleLeft(alternate))
        .toRight(MosMaskProblem.MissingKeyword(primary))

    def parse[A](
      key:     String,
      raw:     String,
      degrees: Double => Option[A],
      hmsDms:  String => Option[A]
    ): Either[MosMaskProblem, A] =
      h.double(key)
        .flatMap(degrees)
        .orElse(hmsDms(FitsHeader.unquote(raw)))
        .toRight(MosMaskProblem.InvalidKeyword(key, FitsHeader.unquote(raw)))

    for
      (raKey, raRaw)   <- keyword("RA_IMAG", "RA")
      (decKey, decRaw) <- keyword("DEC_IMAG", "DEC")
      ra               <- parse(raKey,
                                raRaw,
                                d => RightAscension.fromDoubleDegrees(d).some,
                                RightAscension.lenientFromStringHMS.getOption
                          )
      dec              <- parse(decKey,
                                decRaw,
                                Declination.fromDoubleDegrees,
                                Declination.lenientFromStringDMS.getOption
                          )
    yield Coordinates(ra, dec)

  /**
   * Nod & Shuffle configuration.
   *
   * The keywords appear in fixed groups, so an unrecognised or absent `SHUFMODE` means the design
   * is simply not a Nod & Shuffle one.
   */
  private def decodeNodAndShuffle(h: FitsHeader): MosNodAndShuffle =
    val shuffle = h.int("SHUFSIZE").getOrElse(0)
    val binning = h.int("BINNING").getOrElse(1)
    h.nonBlank("SHUFMODE") match
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
      filter = h.nonBlank("FILTSPEC"),
      grating = h.nonBlank("GRATING"),
      centralWavelength = nm("WAVELENG"),
      minWavelength = nm("SPEC_MIN"),
      maxWavelength = nm("SPEC_MAX"),
      dispersion = h.double("SPEC_DIS").map(BigDecimal(_).withUnit[NanometersPerPixel]),
      spectrumLength = h.double("SPEC_LEN").map(BigDecimal(_).withUnit[Pixels]),
      anamorphicFactor = h.double("ANAMORPH")
    )

  private def decodeProvenance(h: FitsHeader): MosMaskProvenance =
    MosMaskProvenance(
      softwareVersion = h.nonBlank("GMMPSVER"),
      designer = h.nonBlank("PERS_ODF"),
      designedAt = h.nonBlank("DATE_ODF").flatMap(parseTimestamp),
      sourceObjectTable = h.nonBlank("FILE_OT"),
      detectorIdImaging = h.nonBlank("DET_IMG"),
      detectorIdSpectroscopy = h.nonBlank("DET_SPEC")
    )

  /** `DATE_ODF` is an ISO-8601 local date and time, without a zone. */
  private def parseTimestamp(s: String): Option[Timestamp] =
    try Timestamp.fromLocalDateTime(LocalDateTime.parse(s.trim, DateTimeFormatter.ISO_DATE_TIME))
    catch case NonFatal(_) => none

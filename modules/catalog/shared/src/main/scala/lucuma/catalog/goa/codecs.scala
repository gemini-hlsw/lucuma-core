// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField
import java.time.temporal.TemporalQueries

object codecs:

  // GOA emits UT timestamps as "yyyy-MM-dd HH:mm:ss" with an optional
  // fractional-seconds part (any precision) and an optional zone offset.
  private val utDateTimeFormatter: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendPattern("yyyy-MM-dd HH:mm:ss")
      .optionalStart()
      .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
      .optionalEnd()
      .optionalStart()
      .appendOffsetId()
      .optionalEnd()
      .toFormatter()

  private val releaseDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ISO_LOCAL_DATE

  given Decoder[Instant] = Decoder.decodeString.emap: s =>
    Either
      .catchNonFatal:
        val parsed = utDateTimeFormatter.parse(s)
        val local  = LocalDateTime.from(parsed)
        val offset = Option(parsed.query(TemporalQueries.offset)).getOrElse(ZoneOffset.UTC)
        local.toInstant(offset)
      .leftMap(e => s"Invalid datetime: ${e.getMessage}")

  given Decoder[LocalDate] = Decoder.decodeString.emap: s =>
    Either
      .catchNonFatal(LocalDate.parse(s, releaseDateFormatter))
      .leftMap(e => s"Invalid date: ${e.getMessage}")

  extension (c: HCursor)
    private def getOptA[A](
      fieldName: String
    )(parse: Double => Option[A]): Decoder.Result[Option[A]] =
      c.get[Option[Double]](fieldName)
        .flatMap:
          case None    => Right(None)
          case Some(d) =>
            parse(d)
              .map(Some(_))
              .toRight(DecodingFailure(s"Invalid $fieldName: $d", c.history))

  given Decoder[GoaSummaryRecord] = (c: HCursor) =>
    for
      name             <- c.get[String]("name")
      dataLabel        <- c.get[Option[String]]("data_label")
      ra               <- c.getOptA("ra")(d => Some(RightAscension.fromDoubleDegrees(d)))
      dec              <- c.getOptA("dec")(Declination.fromDoubleDegrees)
      instrument       <- c.get[String]("instrument")
      observationType  <- c.get[String]("observation_type").map(GoaObservationType.fromTag)
      observationClass <-
        c.get[Option[String]]("observation_class").map(_.map(GoaObservationClass.fromTag))
      qaState          <- c.get[Option[String]]("qa_state")
      utDateTime       <- c.get[Option[Instant]]("ut_datetime")
      releaseDate      <- c.get[Option[LocalDate]]("release")
      programId        <- c.get[Option[String]]("program_id")
      observationId    <- c.get[Option[String]]("observation_id")
      objectName       <- c.get[Option[String]]("object")
      exposure         <- c.getOptA("exposure_time")(d => TimeSpan.fromSeconds(BigDecimal(d)))
      disperser        <- c.get[Option[String]]("disperser")
      filter           <- c.get[Option[String]]("filter_name")
      wavelength       <-
        c.getOptA("central_wavelength")(d => Wavelength.decimalMicrometers.getOption(BigDecimal(d)))
      airmass          <- c.get[Option[Double]]("airmass")
      azimuth          <- c.getOptA("azimuth")(d => Some(Angle.fromDoubleDegrees(d)))
      elevation        <- c.getOptA("elevation")(d => Some(Angle.fromDoubleDegrees(d)))
    yield GoaSummaryRecord(
      name,
      dataLabel,
      ra,
      dec,
      instrument,
      observationType,
      observationClass,
      qaState,
      utDateTime,
      releaseDate,
      programId,
      observationId,
      objectName,
      exposure,
      disperser,
      filter,
      wavelength,
      airmass,
      azimuth,
      elevation
    )

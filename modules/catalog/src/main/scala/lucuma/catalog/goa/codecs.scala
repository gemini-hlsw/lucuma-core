// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.HCursor

import java.time.LocalDate
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object codecs:

  private val utDateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss[.SSSSSS]")

  private val releaseDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ISO_LOCAL_DATE

  given Decoder[LocalDateTime] = Decoder.decodeString.emap: s =>
    Either.catchNonFatal(LocalDateTime.parse(s, utDateTimeFormatter))
      .leftMap(e => s"Invalid datetime: ${e.getMessage}")

  given Decoder[LocalDate] = Decoder.decodeString.emap: s =>
    Either.catchNonFatal(LocalDate.parse(s, releaseDateFormatter))
      .leftMap(e => s"Invalid date: ${e.getMessage}")

  given Decoder[GoaSummaryRecord] = (c: HCursor) =>
    for
      name             <- c.get[String]("name")
      dataLabel        <- c.get[Option[String]]("data_label")
      ra               <- c.get[Option[Double]]("ra")
      dec              <- c.get[Option[Double]]("dec")
      instrument       <- c.get[String]("instrument")
      observationType  <- c.get[String]("observation_type")
      observationClass <- c.get[Option[String]]("observation_class")
      qaState          <- c.get[Option[String]]("qa_state")
      utDateTime       <- c.get[Option[LocalDateTime]]("ut_datetime")
      releaseDate      <- c.get[Option[LocalDate]]("release")
      programId        <- c.get[Option[String]]("program_id")
      observationId    <- c.get[Option[String]]("observation_id")
      objectName       <- c.get[Option[String]]("object")
      exposure         <- c.get[Option[Double]]("exposure_time")
      disperser        <- c.get[Option[String]]("disperser")
      filter           <- c.get[Option[String]]("filter_name")
      wavelength       <- c.get[Option[Double]]("central_wavelength")
      airmass          <- c.get[Option[Double]]("airmass")
      azimuth          <- c.get[Option[Double]]("azimuth")
      elevation        <- c.get[Option[Double]]("elevation")
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

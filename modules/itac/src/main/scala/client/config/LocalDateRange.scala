// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.config

import cats.implicits.*
import io.circe.*
import java.time.LocalDate
import java.time.format.DateTimeFormatter.BASIC_ISO_DATE
import java.time.format.DateTimeParseException

sealed abstract case class LocalDateRange(
  start: LocalDate,
  end:   LocalDate
) {
  // Sanity check, should be correct on construction
  assert(!end.isBefore(start))

}

object LocalDateRange {

  def apply(start: LocalDate, end: LocalDate): Either[String, LocalDateRange] =
    if (end.isBefore(start)) Left(s"$end is before $start.")
    else Right(new LocalDateRange(start, end) {})

  implicit lazy val EncodeLocalDateRange: Encoder[LocalDateRange] =
    Encoder[String].contramap { case LocalDateRange(a, b) =>
      s"${a.format(BASIC_ISO_DATE)}-${b.format(BASIC_ISO_DATE)}"
    }

  implicit lazy val DecodeLocalDateRange: Decoder[LocalDateRange] =
    Decoder[String].emap { case s =>
      s.split("-") match {
        case Array(sa, sb) =>
          Either.catchOnly[DateTimeParseException] {
            (LocalDate.parse(sa, BASIC_ISO_DATE), LocalDate.parse(sb, BASIC_ISO_DATE))
          } .leftMap(_.getMessage)
            .flatMap { case (a, b) => apply(a, b) }
        case _ => Left(s"Invalid date range: $s")
      }
    }

  // /** Dummy date range at mid-semester, for `init` when no prior config is available. */
  // def dummy(semester: Semester, site: Site): LocalDateRange = {
  //   val inst  = Instant.ofEpochMilli(semester.getMidpointDate(site).getTime)
  //   val start = inst.atZone(site.timezone.toZoneId).toLocalDate
  //   apply(start, start.plusDays(2)).toOption.get
  // }

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.syntax.all.*
import org.typelevel.cats.time.*

import java.time.Duration
import java.time.Instant
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.OffsetTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal
import java.time.temporal.TemporalUnit

object time:
  private def roundTemporalTo[T <: Temporal](
    temporal: T,
    units: TemporalUnit,
    truncate: (T, TemporalUnit) => T,
    plus: (T, Long, TemporalUnit) => T,
    repeatEvery: Option[Duration] = none
  ): T =
      val floor = truncate(temporal, units)
      val ceil = plus(floor, 1, units)
      val distToFloor = Duration.between(floor, temporal)
      val distToCeil = Duration.between(temporal, ceil)
      // Types with just time and no date repeat themselves, so we have to take that into account.
      val distToCeilFixed = if(distToCeil.isNegative) repeatEvery.get.plus(distToCeil) else distToCeil
      if( distToFloor < distToCeilFixed) floor else ceil

  extension (zdt: ZonedDateTime)
    /** Round a ZonedDateTime to the nearest value of the specified unit. */
    def roundTo(units: TemporalUnit): ZonedDateTime =
      roundTemporalTo(zdt, units, _.truncatedTo(_), _.plus(_, _))

  extension (i: Instant)
    /** Round an Instant to the nearest value of the specified unit. */
    def roundTo(units: TemporalUnit): Instant =
      roundTemporalTo(i, units, _.truncatedTo(_), _.plus(_, _))

  extension (ldt: LocalDateTime)
    /** Round a LocalDateTime to the nearest value of the specified unit. */
    def roundTo(units: TemporalUnit): LocalDateTime =
      roundTemporalTo(ldt, units, _.truncatedTo(_), _.plus(_, _))

  extension (lt: LocalTime)
    /** Round a LocalTime to the nearest value of the specified unit. */
    def roundTo(units: TemporalUnit): LocalTime =
      roundTemporalTo(lt, units, _.truncatedTo(_), _.plus(_, _), Duration.ofDays(1).some)

  extension (odt: OffsetDateTime)
    /** Round an OffsetDateTime to the nearest value of the specified unit. */
    def roundTo(units: TemporalUnit): OffsetDateTime =
      roundTemporalTo(odt, units, _.truncatedTo(_), _.plus(_, _))

  extension (ot: OffsetTime)
    /** Round an OffsetTime to the nearest value of the specified unit. */
    def roundTo(units: TemporalUnit): OffsetTime =
      roundTemporalTo(ot, units, _.truncatedTo(_), _.plus(_, _), Duration.ofDays(1).some)

  object format:
    val DateFormat = "yyyy-MMM-dd"
    val TimeFormat = "HH:mm"

    val GppDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern(DateFormat)

    val GppTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern(TimeFormat)

    val GppTimeTZFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern(TimeFormat).withZone(ZoneOffset.UTC)

    val GppTimeTZFormatterWithZone: DateTimeFormatter =
      DateTimeFormatter.ofPattern("HH:mm 'UTC'").withZone(ZoneOffset.UTC)

    val IsoUTCFormatter: DateTimeFormatter =
      DateTimeFormatter.ISO_OFFSET_DATE_TIME.withZone(ZoneOffset.UTC)

    val UtcFormatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneOffset.UTC)

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import cats.syntax.all._
import org.typelevel.cats.time._
import lucuma.core.optics.Spire
import lucuma.core.syntax.boundedInterval._
import spire.math.Bounded
import spire.math.Empty
import spire.math.Point
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalAmount

// A bit of syntax to make working with Java Instant and Duration a bit less
// cumbersome / easier to read.

final class InstantOps(val self: Instant) extends AnyVal {

  def +(t: TemporalAmount): Instant =
    self.plus(t)

  def -(t: TemporalAmount): Instant =
    self.minus(t)

}

trait ToInstantOps {
  implicit def ToInstantOps(i: Instant): InstantOps =
    new InstantOps(i)
}

object instant extends ToInstantOps

final class ZonedDateTimeOps(val self: ZonedDateTime) extends AnyVal {

  def +(t: TemporalAmount): ZonedDateTime =
    self.plus(t)

  def -(t: TemporalAmount): ZonedDateTime =
    self.minus(t)

}

trait ToZonedDateTimeOps {
  implicit def ToZonedDateTimeOps(zdt: ZonedDateTime): ZonedDateTimeOps =
    new ZonedDateTimeOps(zdt)
}

object zonedDateTime extends ToZonedDateTimeOps

final class DurationOps(val self: Duration) extends AnyVal {

  def +(that: Duration): Duration =
    self.plus(that)

  def -(that: Duration): Duration =
    self.minus(that)

  def *(m: Long): Duration =
    self.multipliedBy(m)

  def /(d: Long): Duration =
    self.dividedBy(d)
}

trait ToDurationOps {
  implicit def ToDurationOps(d: Duration): DurationOps =
    new DurationOps(d)
}

object duration extends ToDurationOps

final class InstantBoundedOps(val self: Bounded[Instant]) extends AnyVal {

  /**
   * Convert to the minimal full-day interval that includes this interval.
   *
   * Hours in interval may not be a multiple of 24 if there's a DST transition in the resulting interval.
   *
   * @param zone the timezone where the start of the day should be computed
   * @param startOfDay time at which the day starts
   */
  def toFullDays(zone: ZoneId, startOfDay: LocalTime): Bounded[Instant] = {
    val fullDayStart = {
      val startAtZone = self.lower.atZone(zone)
      val newStart    = startAtZone.`with`(startOfDay)
      if (newStart <= startAtZone) newStart.toInstant
      else newStart.minusDays(1).`with`(startOfDay).toInstant
    }

    val fullDayEnd = {
      val endAtZone = self.upper.atZone(zone)
      val newEnd    = endAtZone.`with`(startOfDay)
      if (newEnd >= endAtZone) newEnd.toInstant
      else newEnd.plusDays(1).`with`(startOfDay).toInstant
    }

    Bounded.unsafeOpenUpper(fullDayStart, fullDayEnd)
  }

  def duration: Duration = {
    val (start, end) = Spire.openUpperIntervalFromTuple[Instant].reverseGet(self)
    Duration.between(start, end)
  }
}

trait ToInstantBoundedOps {
  implicit def ToInstantIntervalOps(i: Bounded[Instant]): InstantBoundedOps =
    new InstantBoundedOps(i)
}

object instantBoundedInterval extends ToInstantBoundedOps

final class InstantIntervalSeqOps(val self: IntervalSeq[Instant]) extends AnyVal {
  import instantBoundedInterval._

  def duration: Duration =
    self.intervals
      .foldLeft(Duration.ZERO.some)((d, i) =>
        i match {
          case b @ Bounded(_, _, _) => d.map(_.plus(b.duration))
          case Point(_)             => d
          case Empty()              => d
          case _                    => none
        }
      )
      .getOrElse(ChronoUnit.FOREVER.getDuration)
}

trait ToInstantIntervalSeqOps {
  implicit def ToInstantIntervalSeqOps(s: IntervalSeq[Instant]): InstantIntervalSeqOps =
    new InstantIntervalSeqOps(s)
}

object instantInterval extends ToInstantIntervalSeqOps

object time
    extends ToInstantOps
    with ToDurationOps
    with ToZonedDateTimeOps
    with ToInstantBoundedOps
    with ToInstantIntervalSeqOps

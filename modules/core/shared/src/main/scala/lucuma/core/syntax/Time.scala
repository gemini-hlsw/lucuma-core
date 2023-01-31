// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.optics.Spire
import lucuma.core.util.TimeSpan
import org.typelevel.cats.time.*
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

trait InstantOps:
  extension (self: Instant)
    def +(t: TemporalAmount): Instant =
      self.plus(t)

    def -(t: TemporalAmount): Instant =
      self.minus(t)

object instant extends InstantOps

trait ZonedDateTimeOps:
  extension (self: ZonedDateTime)
    def +(t: TemporalAmount): ZonedDateTime =
      self.plus(t)

    def -(t: TemporalAmount): ZonedDateTime =
      self.minus(t)

object zonedDateTime extends ZonedDateTimeOps

trait DurationOps:
  extension (self: Duration)
    def +(that: Duration): Duration =
      self.plus(that)

    def -(that: Duration): Duration =
      self.minus(that)

    def *(m: Long): Duration =
      self.multipliedBy(m)

    def /(d: Long): Duration =
      self.dividedBy(d)

    def toMicros: Long = self.toNanos / 1000

object duration extends DurationOps

trait  LongDurationOps:
  extension( self: Long)
    def nanoseconds: Duration =
      Duration.ofNanos(self)

    def microseconds: Duration =
      Duration.ofNanos(self * 1000)

    def milliseconds: Duration =
      Duration.ofMillis(self)

    def seconds: Duration =
      Duration.ofSeconds(self)

    def minutes: Duration =
      Duration.ofMinutes(self)

object longDuration extends LongDurationOps

trait InstantBoundedIntervalOps:
  extension (self: BoundedInterval[Instant])
    /**
     * Convert to the minimal full-day interval that includes this interval.
     *
     * Hours in interval may not be a multiple of 24 if there's a DST transition in the resulting
     * interval.
     *
     * @param zone
     *   the timezone where the start of the day should be computed
     * @param startOfDay
     *   time at which the day starts
     */
    def toFullDays(zone: ZoneId, startOfDay: LocalTime): BoundedInterval[Instant] = {
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

      BoundedInterval.unsafeOpenUpper(fullDayStart, fullDayEnd)
    }

    def duration: Duration = {
      val (start, end) = Spire.openUpperIntervalFromTuple[Instant].reverseGet(self)
      Duration.between(start, end)
    }

object instantBoundedInterval extends InstantBoundedIntervalOps

trait InstantIntervalSeqOps extends InstantBoundedIntervalOps:

  extension (self: IntervalSeq[Instant])
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


object instantInterval extends InstantIntervalSeqOps

trait TimeOps
    extends InstantOps
    with DurationOps
    with LongDurationOps
    with ZonedDateTimeOps
    with InstantIntervalSeqOps
    
object time extends TimeOps
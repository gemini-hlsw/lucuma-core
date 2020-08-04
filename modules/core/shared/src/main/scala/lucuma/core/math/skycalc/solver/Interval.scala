// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import cats.Eq
import cats.Order
import cats.Show
import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneId
import monocle.Getter
import monocle.Prism
import gsp.math.optics.Format
import io.chrisdavenport.cats.time._

/**
  * Representation of an interval between two points in time, including the start time and excluding the end time
  * (i.e. the interval [start, end)).
  *
  * Note that an interval can not represent a single point in time (i.e. start == end) because such an interval
  * could not contain any time t for which t >= start && t < end.
  */
sealed abstract case class Interval protected (start: Instant, end: Instant) {
  // Extra checks. Should be correct via the companion constructors.
  require(start.isBefore(end), "start of interval must be < end")

  /** The duration of this interval. */
  lazy val duration: Duration = Duration.between(start, end)

  /** True if this interval covers time t. */
  def contains(i: Instant): Boolean = i >= start && i < end

  /** True if this interval covers the given interval. */
  def contains(i: Interval): Boolean = i.start >= start && i.end <= end

  /** True if this and the other interval abut each other. */
  def abuts(other: Interval): Boolean =
    start == other.end || other.start == end

  /** True if this and the other interval overlap each other either fully or partially. */
  def overlaps(other: Interval): Boolean =
    start < other.end && end > other.start

  /** Join two abutting or overlapping intervals.
    *
    * This operation is only defined if the two intervals overlap
    * or abut each other, i.e. in all cases where adding the two intervals results in one single interval.
    */
  def join(other: Interval): Option[Interval] =
    if (overlaps(other) || abuts(other))
      Interval(start.min(other.start), end.max(other.end))
    else
      none

  /** The overlapping part of two intervals. */
  def intersection(other: Interval): Option[Interval] =
    if (overlaps(other))
      Interval(start.max(other.start), end.min(other.end))
    else
      none

  /** The result of removing one interval from another, which can result in zero, one or two intervals */
  def diff(other: Interval): List[Interval] =
    if (this.start < other.start)
      if (other.end < this.end)
        List(Interval.unsafe(this.start, other.start), Interval.unsafe(other.end, this.end))
      else if (other.start <= this.end)
        List(Interval.unsafe(this.start, other.start))
      else
        List(this)
    else if (other.end <= this.start)
      List(this)
    else if (other.end < this.end)
      List(Interval.unsafe(other.end, this.end))
    else
      List.empty

  /** The result of removing the intervals from a Schedule from this interval */
  def diff(other: Schedule): Schedule =
    Schedule.single(this).diff(other)

  /** Convert to the minimal full-day interval that includes this interval.
    *
    * @param zone the timezone where the start of the day should be computed
    * @param startOfDay time at which the day starts
    */
  def toFullDays(zone: ZoneId, startOfDay: LocalTime): Interval = {
    val startAtZone = start.atZone(zone)
    val newStart    = startAtZone.`with`(startOfDay)
    val endAtZone   = end.atZone(zone)
    val newEnd      = endAtZone.`with`(startOfDay)
    Interval.unsafe(
      if (newStart <= startAtZone) newStart.toInstant else newStart.minusDays(1).toInstant,
      if (newEnd >= endAtZone) newEnd.toInstant else newEnd.plusDays(1).toInstant
    )
  }
}

object Interval extends IntervalOptics {

  /** @group Constants */
  val Always: Interval = unsafe(Instant.MIN, Instant.MAX)

  /**
    * Construct a new Interval with specified start and end instants, as long as start < end.
    * @group Constructors
    */
  def apply(start: Instant, end: Instant): Option[Interval] =
    fromOrderedInstants.getOption((start, end))

  /**
    * Construct a new Interval with specified start and end instants, throwing an exception if start >= end.
    * @group Constructors
    */
  def unsafe(start: Instant, end: Instant): Interval =
    apply(start, end).get

  /**
    * Construct a new Interval with specified start instant and duration, as long as duration >= 0.
    * @group Constructors
    */
  def apply(start: Instant, duration: Duration): Option[Interval] =
    fromStartDuration.getOption((start, duration))

  /** @group Typeclass Instances */
  implicit val IntervalShow: Show[Interval] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val IntervalEqual: Eq[Interval] =
    Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit val IntervalOrder: Order[Interval] =
    Order.by(i => (i.start, i.end))
}

trait IntervalOptics { self: Interval.type =>

  /** @group Optics */
  val start: Getter[Interval, Instant] =
    Getter(_.start)

  /** @group Optics */
  val end: Getter[Interval, Instant] =
    Getter(_.end)

  /** @group Optics */
  val duration: Getter[Interval, Duration] =
    Getter(_.duration)

  /** A tuple of Instants corresponding to (start, end).
    * @group Optics
    */
  val fromOrderedInstants: Prism[(Instant, Instant), Interval] =
    Prism[(Instant, Instant), Interval] {
      case (start: Instant, end: Instant) =>
        if (start < end) (new Interval(start, end) {}).some else none
    }(i => (i.start, i.end))

  /** A tuple of Instants, which will be sorted if necessary. Can still fail if they are both the same.
    * @group Optics
    */
  val fromInstants: Format[(Instant, Instant), Interval] =
    Format[(Instant, Instant), Interval](
      {
        case (start: Instant, end: Instant) =>
          if (start < end) (new Interval(start, end) {}).some
          else if (start > end) (new Interval(end, start) {}).some
          else none
      },
      i => (i.start, i.end)
    )

  /** A tuple containing the start Instant and the Duration of the Interval. Can still fail if duration <= 0.
    *  @group Optics
    */
  val fromStartDuration: Prism[(Instant, Duration), Interval] =
    Prism[(Instant, Duration), Interval] {
      case (start, duration) =>
        if (duration > Duration.ZERO)
          (new Interval(start, start.plus(duration)) {}).some
        else
          none
    }(i => (i.start, i.duration))

}

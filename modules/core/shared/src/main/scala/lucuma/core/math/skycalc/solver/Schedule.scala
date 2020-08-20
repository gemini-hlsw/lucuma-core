// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import cats.Eq
import cats.Show
import cats.kernel.BoundedSemilattice
import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneId
import monocle.Getter
import monocle.Prism
import gsp.math.optics.SplitEpi
import io.chrisdavenport.cats.time._
import scala.annotation.tailrec

/**
  * A sequence of Intervals.
  *
  * The intervals are sorted by their start time and don't overlap or abut, i.e. the Schedule is always represented
  * by the smallest possible set of Intervals.
  */
sealed abstract case class Schedule protected (intervals: List[Interval]) {

  lazy val isEmpty: Boolean             = intervals.isEmpty
  lazy val nonEmpty: Boolean            = !isEmpty
  lazy val never: Boolean               = isEmpty
  lazy val headOption: Option[Interval] = intervals.headOption
  lazy val tail: Schedule               =
    intervals match {
      case _ :: t => Schedule.unsafe(t)
      case Nil    => Schedule.Never
    }
  lazy val earliest: Option[Instant]    = headOption.map(_.start)
  lazy val latest: Option[Instant]      = intervals.lastOption.map(_.end)
  lazy val duration: Duration           = intervals.foldMap(_.duration)

  /** True if the Schedule (i.e. any of its intervals) contains instant i. */
  def contains(i: Instant): Boolean = intervals.exists(_.contains(i))

  /** True if the Schedule (i.e. any of its intervals) covers interval i. */
  def covers(i: Interval): Boolean = intervals.exists(_.contains(i))

  /** True if any interval of this Schedule overlaps with the given interval. */
  def overlaps(interval: Interval): Boolean =
    intervals.exists(i => i.overlaps(interval))

  /** Restricts a Schedule to only the intervals in the given interval. */
  def restrictTo(interval: Interval): Schedule =
    Schedule.unsafe(
      intervals
        .filter(i => i.end > interval.start && i.start < interval.end)
        .map(i => Interval.unsafe(i.start.max(interval.start), i.end.min(interval.end)))
    )

  /** Map intervals. Sort and join them when necessary, to produce a valid Schedule. */
  def mapIntervals(f: Interval => Interval): Schedule =
    Schedule.fromIntervals.get(intervals.map(f))

  /** Convert to the minimal Schedule containing only full days that covers this Schedule.
    *
    * @param zone the timezone where the start of the day should be computed
    * @param startOfDay time at which the day starts
    */
  def toFullDays(zone: ZoneId, startOfDay: LocalTime): Schedule =
    mapIntervals(_.toFullDays(zone, startOfDay))

  /** Compute minimal Schedule that contains all instants from either Schedule. */
  def union(other: Schedule): Schedule = Schedule.union(this, other)

  /** Compute minimal Schedule that contains all instants from this Schedule and the Interval. */
  def union(interval: Interval): Schedule = union(Schedule.single(interval))

  /** Compute minimal Schedule that contains all instants covered in both Schedules. */
  def intersection(other: Schedule): Schedule = Schedule.intersection(this, other)

  /** Compute minimal Schedule that contains all instants covered in both this Schedule and the Interval. */
  def intersection(interval: Interval): Schedule = intersection(Schedule.single(interval))

  /** Remove from this Schedule the intersection with another one.
    *
    * The result will cover all instants of this Schedule which are NOT covered by the the given Schedule.
    */
  def diff(other: Schedule): Schedule = Schedule.diff(this, other)

  /** Remove from this Schedule the intersection with an Interval.
    *
    * The result will cover all instants of this Schedule which are NOT covered by the the given Interval.
    */
  def diff(interval: Interval): Schedule = diff(Schedule.single(interval))

  /** Compute a Schedule covering all instants in the same timespan but not covered by this Schedule. */
  def gaps: Schedule =
    if (intervals.size < 2) Schedule.Never
    else
      Schedule.unsafe(
        intervals
          .sliding(2)
          .map { case List(i, j) => Interval.unsafe(i.end, j.start) }
          .toList
      )
}

object Schedule extends ScheduleOptics {

  /**
    * Schedule that covers all possible instants.
    * @group Constants
    */
  val Always = unsafe(List(Interval.Always))

  /**
    * Schedule that doesn't cover any instants.
    * @group constants
    */
  val Never = unsafe(List.empty)

  /**
    * Construct a new Schedule containing only the given Interval.
    * @group Constructors
    */
  def single(interval: Interval): Schedule = new Schedule(List(interval)) {}

  /**
    * Construct a new empty Schedule.
    * @group Constructors
    */
  def apply(): Schedule = Never

  /**
    * Construct a new Schedule containing only the given Interval.
    * @group Constructors
    */
  def apply(interval: Interval): Schedule = single(interval)

  /**
    * Construct a new Schedule containing a single Interval between the given Instants.
    *
    * Returns None if start >= end.
    *
    * @group Constructors
    */
  def apply(start: Instant, end: Instant): Option[Schedule] =
    Interval(start, end).map(single)

  /**
    * Construct a new Schedule containing the given Intervals.
    *
    * Returns None if the Intervals are not sorted or if any two of them abut or overlap.
    *
    * @group Constructors
    */
  def apply(intervals: List[Interval]): Option[Schedule] =
    fromDisjointSortedIntervals.getOption(intervals)

  /**
    * Construct a new Schedule containing a single Interval between the given Instants.
    *
    * Fails if start >= end.
    *
    * @group Constructors
    */
  def unsafe(start: Instant, end: Instant): Schedule =
    apply(start, end).get

  /**
    * Construct a new Schedule containing the given Intervals.
    *
    * Fails if the Intervals are not sorted or if any two of them abut or overlap.
    *
    * @group Constructors
    */
  def unsafe(intervals: List[Interval]): Schedule =
    apply(intervals).get

  /** Helper functions. In recursions, reversed accumulators are used to avoid incurring in quadratic costs. */
  private def union(s1: Schedule, s2: Schedule): Schedule = {

    @tailrec
    def joinInitial(
      head: Interval,
      tail: List[Interval]
    ): List[Interval] =
      tail match {
        case Nil                  => List(head)
        case tailHead :: tailTail =>
          head.join(tailHead) match {
            case None         => head +: tail
            case Some(joined) => joinInitial(joined, tailTail)
          }
      }

    @tailrec
    def go(
      left:  List[Interval],
      right: List[Interval],
      accum: List[Interval]
    ): List[Interval] =
      (left, right) match {
        case (Nil, _)                        => right.reverse ++ accum
        case (_, Nil)                        => left.reverse ++ accum
        case (leftHead :: _, rightHead :: _) =>
          leftHead.join(rightHead) match {
            case None         => // Heads can't be joined: they don't abut or overlap
              if (leftHead.start < rightHead.start)
                go(left.tail, right, leftHead :: accum)
              else
                go(left, right.tail, rightHead :: accum)
            case Some(joined) =>
              // The new Interval could overlap with other Intervals on the left Schedule, so we
              // combine it with the rest of the left list before proceeding.
              go(joinInitial(joined, left.tail), right.tail, accum)
          }
      }

    unsafe(go(s1.intervals, s2.intervals, List.empty).reverse)
  }

  private def intersection(s1: Schedule, s2: Schedule): Schedule = {

    @tailrec
    def go(
      left:  List[Interval],
      right: List[Interval],
      accum: List[Interval]
    ): List[Interval] =
      (left, right) match {
        case (Nil, _)                        => accum
        case (_, Nil)                        => accum
        case (leftHead :: _, rightHead :: _) =>
          leftHead.intersection(rightHead) match {
            case None              => // Heads can't be joined: they don't about or overlap
              if (leftHead.end > rightHead.end)
                go(left, right.tail, accum)
              else
                go(left.tail, right, accum)
            case Some(intersected) =>
              if (leftHead.end > rightHead.end)
                go(left, right.tail, intersected :: accum)
              else
                go(left.tail, right, intersected :: accum)
          }
      }

    unsafe(go(s1.intervals, s2.intervals, List.empty).reverse)
  }

  private def diff(s1: Schedule, s2: Schedule): Schedule = {
    @tailrec
    def go(left: List[Interval], right: List[Interval], accum: List[Interval]): List[Interval] =
      (left, right) match {
        case (Nil, _)                        => accum
        case (_, Nil)                        => left.reverse ++ accum
        case (leftHead :: _, rightHead :: _) =>
          if (!leftHead.overlaps(rightHead))
            if (leftHead.end <= rightHead.start)
              // no overlap and leftHead is before rightHead => leftHead won't be touched again by any rightHead, add it to result
              go(left.tail, right, leftHead :: accum)
            else
              // no overlap and leftHead is after rightHead => we can skip rightHead
              go(left, right.tail, accum)
          else
            // overlap: replace leftHead with leftHead.diff(rightHead) and continue
            go(leftHead.diff(rightHead) ++ left.tail, right, accum)
      }

    unsafe(go(s1.intervals, s2.intervals, List.empty).reverse)
  }

  /** @group Typeclass Instances */
  implicit val ScheduleShow: Show[Schedule] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val ScheduleEqual: Eq[Schedule] = Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  object UnionBoundedSemilattice extends BoundedSemilattice[Schedule] {
    def empty: Schedule = Schedule.Never

    def combine(x: Schedule, y: Schedule): Schedule = x.union(y)
  }

  /** @group Typeclass Instances */
  object IntersectionBoundedSemilattice extends BoundedSemilattice[Schedule] {
    def empty: Schedule = Schedule.Always

    def combine(x: Schedule, y: Schedule): Schedule = x.intersection(y)
  }
}

trait ScheduleOptics { self: Schedule.type =>

  /** @group Optics */
  val intervals: Getter[Schedule, List[Interval]] =
    Getter(_.intervals)

  /** A list of Intervals, which are sorted and do not abut or overlap each other.
    * @group Optics
    */
  val fromDisjointSortedIntervals: Prism[List[Interval], Schedule] =
    Prism { intervals: List[Interval] =>
      if (
        intervals.sliding(2).forall {
          case List(a, b) => a.end < b.start
          case _          => true // Zero or one intervals
        }
      )
        (new Schedule(intervals) {}).some
      else
        none
    }(_.intervals)

  /** Any list of Intervals. They will be sorted and merged if necessary.
    * @group Optics
    */
  val fromIntervals: SplitEpi[List[Interval], Schedule] =
    SplitEpi[List[Interval], Schedule](
      _.foldLeft(Schedule.Never)((s, i) => s.union(i)),
      _.intervals
    )
}

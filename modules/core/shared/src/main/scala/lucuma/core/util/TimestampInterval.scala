// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Comparison
import cats.Eq
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import org.typelevel.cats.time.*

import java.time.Instant
import scala.concurrent.duration.*

/**
 * A TimestampInterval represents a period of time with a fixed starting point
 * 'start' (inclusive) and an fixed ending point 'end' (exclusive).  In other
 * words, [start, end).  Whereas a TimeSpan focuses solely on an amount of
 * time, the TimestampInterval tracks specific start and end times.
 *
 * @param start time at which the interval begins (inclusive)
 * @param end time at which the interval ends (exclusive)
 */
sealed class TimestampInterval private (val start: Timestamp, val end: Timestamp) {

  import TimestampInterval.between
  import TimestampInterval.Overlap

  assert(start <= end, s"start time ($start) must be <= end time ($end)")

  /**
   * The amount of time represented by the interval.
   */
  def timeSpan: TimeSpan =
    TimeSpan.between(start, end)

  def contains(time: Timestamp): Boolean =
    start <= time && time < end

  def containsInstant(time: Instant): Boolean =
    start.toInstant <= time && time < end.toInstant

  def isEmpty: Boolean =
    start === end

  def nonEmpty: Boolean =
    !isEmpty

  /**
   * The interval that includes both this and 'other' interval along with all
   * timestamps in between.
   */
  def span(other: TimestampInterval): TimestampInterval =
    between(start min other.start, end max other.end)

  /**
   * Merges this interval with the given interval if they overlap or abut,
   * returns the two individual intervals otherwise.
   */
  def plus(other: TimestampInterval): List[TimestampInterval] =
    overlap(other) match {
      case Overlap.None if !abuts(other) => List(this, other).sorted
      case _                             => List(span(other))
    }

  /**
   * Subtracts `other` from this interval returning the `List` of intervals
   * that remain.  For example,
   *
   * <ul>
   * <li>if this interval is a subset of `other`: an empty `List`</li>
   *
   * <li>if this interval overlaps either end of `other`: a singleton
   * containing this interval clipped by `other`</li>
   *
   * <li>if this interval is a proper superset of `other`: an interval
   * containing only points in this interval that are to the left of `other`
   * and another interval containing only points to the right of `other.</li>
   * </ul>
   */
  def minus(other: TimestampInterval): List[TimestampInterval] =
    overlap(other) match {
      case Overlap.LowerPartial    => List(between(start, other.start))
      case Overlap.UpperPartial    => List(between(other.end, end))
      case Overlap.ProperSuperset  => if (other.isEmpty) List(this) else List(between(start, other.start), between(other.end, end)).filter(!_.isEmpty)
      case Overlap.None            => List(this)
      case Overlap.ProperSubset |
           Overlap.Equal           => Nil
    }

  /**
   * Determines whether and how this interval overlaps with the 'other'
   * interval.
   */
  def overlap(other: TimestampInterval): Overlap =
    start.comparison(other.start) match {
      case Comparison.LessThan    =>
        if (end <= other.start) Overlap.None
        else if (end < other.end) Overlap.LowerPartial
        else Overlap.ProperSuperset

      case Comparison.EqualTo     =>
        if (end === other.end) Overlap.Equal
        else if (end < other.end) Overlap.ProperSubset
        else Overlap.ProperSuperset

      case Comparison.GreaterThan =>
        if (start >= other.end) Overlap.None
        else if (end > other.end) Overlap.UpperPartial
        else Overlap.ProperSubset
    }

  /**
   * Two time intervals abut if they fall side by side on the timeline with
   * no intervening timestamps.
   */
  def abuts(other: TimestampInterval): Boolean =
    other.start === end || other.end === start

  /**
   * Determines whether two intervals overlap in any way.
   */
  def intersects(other: TimestampInterval): Boolean =
    overlap(other).intersects

  /**
   * Determine the intersection between 2 intervals, or None if they don't
   * intersect.
   */
  def intersection(other: TimestampInterval): Option[TimestampInterval] =
    overlap(other) match
      case Overlap.None           => None
      case Overlap.LowerPartial   => TimestampInterval.between(other.start, end).some
      case Overlap.UpperPartial   => TimestampInterval.between(start, other.end).some
      case Overlap.Equal          => this.some
      case Overlap.ProperSubset   => this.some
      case Overlap.ProperSuperset => other.some
      
  /**
   * Determine the time between the end of the earlier interval and the
   * start of the later, or TimeSpan.Zero if they intersect or abut.
   */
  def timeBetween(other: TimestampInterval): TimeSpan = 
    if (intersects(other)) TimeSpan.Zero
    else if (this < other) TimeSpan.between(end, other.start)
    else TimeSpan.between(other.end, start)

  def duration: FiniteDuration =
    (end.toEpochMilli - start.toEpochMilli).millis

  override def equals(that: Any): Boolean =
    that match {
      case t: TimestampInterval => start === t.start && end === t.end
      case _                    => false
    }

  override def hashCode: Int =
    start.hashCode() * 31 + end.hashCode();

  override def toString: String =
    s"[${start.format}, ${end.format})"

}

object TimestampInterval {

  /**
   * An interval covering all time (that can be expressed in a Timestamp).
   */
  val All: TimestampInterval =
    between(Timestamp.Min, Timestamp.Max)

  /**
   * Creates an interval between the two timestamps, swapping the order if
   * necessary.  If t0 comes before t1, the interval is [t0, t1).  Otherwise
   * it is [t1, t0).
   */
  def between(t0: Timestamp, t1: Timestamp): TimestampInterval =
    if (t0 <= t1) new TimestampInterval(t0, t1) else new TimestampInterval(t1, t0)

  /**
   * Creates an empty interval with a start and end at the given timestamp.
   */
  def empty(at: Timestamp): TimestampInterval =
    TimestampInterval(at, at)

  /**
   * Creates an interval [t, Max).
   */
  def from(t: Timestamp): TimestampInterval =
    TimestampInterval.between(t, Timestamp.Max)

  /**
   * Creates an interval [Min, t).
   */
  def until(t: Timestamp): TimestampInterval =
    TimestampInterval.between(Timestamp.Min, t)

  /** 
   * Creates an Interval [t + pad, t - pad), truncated and bounded as necessary to
   * remain within the range and resolution of `Timestamp`.
   */
  def around(t: Timestamp, pad: FiniteDuration): TimestampInterval =
    between(
      Timestamp.fromInstantTruncatedAndBounded(t.toInstant.plusMillis(pad.toMillis)),
      Timestamp.fromInstantTruncatedAndBounded(t.toInstant.minusMillis(pad.toMillis))
    )

  given Order[TimestampInterval] =
    Order.whenEqual(Order.by(_.start), Order.by(_.end))

  enum Overlap {

    /** No common timestamps between the two intervals. */
    case None

    /** This interval overlaps only the lower end of the other interval. */
    case LowerPartial

    /** This interval overlaps only the upper end of the other interval. */
    case UpperPartial

    /** Both intervals contain the same timestamps. */
    case Equal

    /**
     * All the elements of this interval are contained in the other interval,
     * and they are not Equal.  That is, there are elements in the other
     * interval not contained in this interval.
     */
    case ProperSubset

    /**
     * All the elements of the other interval are contained in this interval,
     * but they are not Equal.  That is, there are elements of this interval
     * which are not contained in the other interval.
     */
    case ProperSuperset
  }

  object Overlap {

    given Eq[Overlap] = Eq.fromUniversalEquals

    extension (self: Overlap) {

      /**
       * Returns True if the intervals are overlapping in any way.
       */
      def intersects: Boolean =
        return self =!= None

      /**
       * Returns True if the intervals are only partially overlapping, False
       * if not overlapping or if one totally overlaps the other.
       */
      def isPartial: Boolean =
        self match {
          case LowerPartial | UpperPartial => true
          case _                           => false
        }

      /**
       * Returns True if one interval totally overlaps the other, False if
       * partially overlapping or not overlapping at all.
       */
      def isTotal: Boolean =
       self match {
          case Equal | ProperSubset | ProperSuperset => true
          case _                                     => false
        }

      /**
       * Returns True if the first interval is a subset (partial or equal)
       * of the other.
       */
      def isSubset: Boolean =
        self match {
          case Equal | ProperSubset => true
          case _                    => false
        }

      /**
       * Returns True if the first interval is a superset (partial or equal)
       * of the other.
       */
      def isSuperset: Boolean =
        self match {
          case Equal | ProperSuperset => true
          case _                      => false
        }
    }
  }


}

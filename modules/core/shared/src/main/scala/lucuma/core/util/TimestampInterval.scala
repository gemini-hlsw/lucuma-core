// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Comparison
import cats.Eq
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.order.*
import org.typelevel.cats.time.*

import java.time.Instant

// Open/closed bounds complication.  Always want [start, end).
// Type parameter instead of int value, allows floating point, rational etc.
// 'Point' unnecessary.

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
   * The amount of time represented by the interval, if possible to fit in a
   * TimeSpan.
   */
  def timeSpan: Option[TimeSpan] =
    TimeSpan.between(start, end)

  /**
   * The amount of time represented by the interval, but capped at
   * TimeSpan.Max.  Most TimestampIntervals in practice will fall
   * well short of TimeSpan.Max and this provides a convenient way
   * of working with time spans that avoids Option.
   */
  def boundedTimeSpan: TimeSpan =
    timeSpan.getOrElse(TimeSpan.Max)

  def contains(time: Timestamp): Boolean =
    start <= time && time < end

  def containsInstant(time: Instant): Boolean =
    start.toInstant <= time && time < end.toInstant

  /**
   * The interval that includes both this and 'other' interval along with all
   * timestamps in between.
   */
  def span(other: TimestampInterval): TimestampInterval =
    between(start min other.start, end max other.end)

  def plus(other: TimestampInterval): List[TimestampInterval] =
    overlap(other) match {
      case Overlap.None if !abuts(other) => List(this, other).sorted
      case _                             => List(span(other))
    }

  def minus(other: TimestampInterval): List[TimestampInterval] =
    overlap(other) match {
      case Overlap.LowerPartial   => List(between(start, other.start))
      case Overlap.UpperPartial   => List(between(other.end, end))
      case Overlap.ProperSuperset => List(between(start, other.start), between(other.end, end))
      case Overlap.None           => List(this)
      case Overlap.ProperSubset |
           Overlap.Equal          => Nil
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

  def between(t0: Timestamp, t1: Timestamp): TimestampInterval =
    if (t0 <= t1) new TimestampInterval(t0, t1) else new TimestampInterval(t1, t0)

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

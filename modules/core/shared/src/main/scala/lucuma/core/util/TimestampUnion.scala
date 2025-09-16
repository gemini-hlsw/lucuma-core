// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.kernel.BoundedSemilattice
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*

import scala.collection.immutable.SortedSet

sealed class TimestampUnion private (val intervals: SortedSet[TimestampInterval]) {

  import TimestampUnion.Empty

  /**
   * Adds the specified interval, which will be merged into the collection
   * of intervals.
   */
  def add(n: TimestampInterval): TimestampUnion = {
    val (u, nʹ) = intervals.foldLeft((Empty, n)) { case ((res, nʹ), ti) =>
      if (nʹ.abuts(ti) || nʹ.intersects(ti)) (res, nʹ.span(ti)) else (res + ti, nʹ)
    }
    new TimestampUnion(u.intervals + nʹ)
  }

  /**
   * Alias for add.
   */
  def +(n: TimestampInterval): TimestampUnion =
    add(n)

  def ++(ns: IterableOnce[TimestampInterval]): TimestampUnion =
    ns.iterator.foldLeft(this)(_.add(_))

  /**
   * Removes the specified interval, which will be clipped out of the
   * collection of intervals.
   */
  def remove(del: TimestampInterval): TimestampUnion =
    intervals.foldLeft(Empty) { (res, ti) => res ++ ti.minus(del) }

  /**
   * Alias for remove.
   */
  def -(del: TimestampInterval): TimestampUnion =
    remove(del)

  def --(ns: IterableOnce[TimestampInterval]): TimestampUnion =
    ns.iterator.foldLeft(this)(_.remove(_))

  /**
   * Intersection of two TimestampUnions, creating a new union with intervals
   * whose times are covered in both.
   */
  def intersect(other: TimestampUnion): TimestampUnion = {
    def collectPoints(init: Vector[Timestamp], intervals: SortedSet[TimestampInterval]): Vector[Timestamp] =
      intervals.foldLeft(init) { (v, ti) => v.appended(ti.start).appended(ti.end) }

    val points = collectPoints(collectPoints(Vector.empty, intervals), other.intervals).sorted

    if (points.isEmpty)
      Empty
    else
      points.zip(points.tail).foldLeft(Empty) { case (res, (a, b)) =>
        if (contains(a) && other.contains(a)) res.add(TimestampInterval.between(a, b)) else res
      }
  }

  /**
   * Alias for intersect.
   */
  def ∩(other: TimestampUnion): TimestampUnion =
    intersect(other)

  def ++(other: TimestampUnion): TimestampUnion =
    ++(other.intervals)

  def --(other: TimestampUnion): TimestampUnion =
    --(other.intervals)

  def contains(timestamp: Timestamp): Boolean =
    intervals.exists(_.contains(timestamp))

  /**
   * Sums the time spanned by all the intervals in this union, assuming the
   * result fits in a `TimeSpan`.
   */
  def sum: Option[TimeSpan] =
    intervals.toList
      .map(_.timeSpan)
      .foldLeft(TimeSpan.Zero.some) { case (s, t) => s.flatMap(_.add(t)) }

  /**
   * Sums the time spanned by all the intervals in this union, capping the
   * result at `TimeSpan.Max`.  This is sufficient for most uses and more
   * convenient than `sum`.
   */
  def boundedSum: TimeSpan =
    intervals.foldMap(_.timeSpan)

  def isEmpty: Boolean =
    intervals.isEmpty

  override def equals(that: Any): Boolean =
    that match {
      case u: TimestampUnion => intervals === u.intervals
      case _                 => false
    }

  override def hashCode: Int =
    intervals.hashCode() * 31

  override def toString: String =
    s"{${intervals.mkString(", ")}}"

}

object TimestampUnion {

  /**
   * The empty TimestampUnion.
   */
  val Empty: TimestampUnion =
    new TimestampUnion(SortedSet.empty)

  def apply(intervals: TimestampInterval*): TimestampUnion =
    fromIntervals(intervals)

  def fromIntervals(intervals: IterableOnce[TimestampInterval]): TimestampUnion =
    Empty ++ intervals

  given Eq[TimestampUnion] =
    Eq.by(_.intervals)

  given BoundedSemilattice[TimestampUnion] =
    BoundedSemilattice.instance(Empty, _ ++ _)

}

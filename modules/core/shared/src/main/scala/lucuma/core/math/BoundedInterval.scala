// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import algebra.ring.AdditiveMonoid
import cats.Eq
import cats.Order
import cats.syntax.all.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.optics.Wedge
import lucuma.core.refined.auto.*
import spire.math.Bounded
import spire.math.Interval
import spire.math.Point
import spire.math.extras.interval.IntervalSeq
import spire.math.interval.ValueBound

/**
  * A `BoundedInterval` is a `spire.math.Interval` but with both bounds defined.
  * Therefore, it can only be a `Bounded` or `Point`. It simplifies usage,
  * without having to worry about the `Empty`, `All`, `Above` and `Below` cases.
  */
opaque type BoundedInterval[A] = Bounded[A] | Point[A]

object BoundedInterval:
  def fromInterval[A](interval: Interval[A]): Option[BoundedInterval[A]] =
    interval.some.filterNot(_.isEmpty).filter(_.isBounded).map(_.asInstanceOf[BoundedInterval[A]])

  def closed[A: Order](lower: A, upper: A): Option[BoundedInterval[A]] =
    fromInterval(Interval.closed(lower, upper))

  def unsafeClosed[A: Order](lower: A, upper: A): BoundedInterval[A] =
    closed(lower, upper).getOrElse(sys.error(s"Could not build a bounded closed interval with lower bound [$lower] and upper bound [$upper]."))

  def open[A: Order](lower: A, upper: A): Option[BoundedInterval[A]] =
    fromInterval(Interval.open(lower, upper))

  def unsafeOpen[A: Order](lower: A, upper: A): BoundedInterval[A] =
    open(lower, upper).getOrElse(sys.error(s"Could not build a bounded open interval with lower bound [$lower] and upper bound [$upper]."))

  def openLower[A: Order](lower: A, upper: A): Option[BoundedInterval[A]] =
    fromInterval(Interval.openLower(lower, upper))

  def unsafeOpenLower[A: Order](lower: A, upper: A): BoundedInterval[A] =
    openLower(lower, upper).getOrElse(sys.error(s"Could not build a bounded open lower interval with lower bound [$lower] and upper bound [$upper]."))

  def openUpper[A: Order](lower: A, upper: A): Option[BoundedInterval[A]] =
    fromInterval(Interval.openUpper(lower, upper))

  def unsafeOpenUpper[A: Order](lower: A, upper: A): BoundedInterval[A] =
    openUpper(lower, upper).getOrElse(sys.error(s"Could not build a bounded open upper interval with lower bound [$lower] and upper bound [$upper]."))

  def fromBounds[A: Order](lower: ValueBound[A], upper: ValueBound[A]): Option[BoundedInterval[A]] =
    fromInterval(Interval.fromBounds(lower, upper))

  def unsafeFromBounds[A: Order](lower: ValueBound[A], upper: ValueBound[A]): BoundedInterval[A] =
    fromBounds(lower, upper).getOrElse(sys.error(s"Could not build a bounded interval with lower bound [$lower] and upper bound [$upper]."))

  given [A: Eq]: Eq[BoundedInterval[A]] = Interval.eq[A].contramap(identity) // contramap needed to fix the type

  extension [A](self: BoundedInterval[A]) {
    def toInterval: Interval[A] = self

    def lowerBound: ValueBound[A] = self match
      case b @ Bounded(_, _, _) => b.lowerBound
      case p @ Point(_) => p.lowerBound

    def upperBound: ValueBound[A] = self match
         case b @ Bounded(_, _, _) => b.upperBound
         case p @ Point(_) => p.upperBound

    def lower: A = self match
      case b @ Bounded(_, _, _) => b.lower
      case p @ Point(_) => p.value

    def upper: A = self match
      case b @ Bounded(_, _, _) => b.upper
      case p @ Point(_) => p.value

    inline def isPoint: Boolean = self.isPoint

    inline def contains(t: A)(implicit o: Order[A]): Boolean = self.contains(t)

    inline def doesNotContain(t: A)(implicit o: Order[A]): Boolean = self.doesNotContain(t)

    inline def crosses(t: A)(implicit o: Order[A]): Boolean = self.crosses(t)

    inline def crossesZero(implicit o: Order[A], ev: AdditiveMonoid[A]): Boolean = self.crossesZero

    inline def mapBounds[B: Order](f: A => B): BoundedInterval[B] = self.mapBounds(f).asInstanceOf[BoundedInterval[B]]

    inline def isSupersetOf(rhs: BoundedInterval[A])(implicit o: Order[A]): Boolean = self.isSupersetOf(rhs)
    inline def isSupersetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean = self.isSupersetOf(rhs)

    inline def isProperSupersetOf(rhs: BoundedInterval[A])(implicit o: Order[A]): Boolean = self.isProperSupersetOf(rhs)
    inline def isProperSupersetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean = self.isProperSupersetOf(rhs)

    inline def isSubsetOf(rhs: BoundedInterval[A])(implicit o: Order[A]): Boolean = self.isSubsetOf(rhs)
    inline def isSubsetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean = self.isSubsetOf(rhs)

    inline def isProperSubsetOf(rhs: BoundedInterval[A])(implicit o: Order[A]): Boolean = self.isProperSubsetOf(rhs)
    inline def isProperSubsetOf(rhs: Interval[A])(implicit o: Order[A]): Boolean = self.isProperSubsetOf(rhs)

    inline def isAt(t: A)(implicit o: Eq[A]): Boolean = self.isAt(t)

    inline def intersects(rhs: BoundedInterval[A])(implicit o: Order[A]): Boolean = self.intersects(rhs)
    inline def intersects(rhs: Interval[A])(implicit o: Order[A]): Boolean = self.intersects(rhs)

    inline def &(rhs: BoundedInterval[A])(implicit o: Order[A]): Option[BoundedInterval[A]] =
      BoundedInterval.fromInterval(&(rhs))
    inline def &(rhs: Interval[A])(implicit o: Order[A]): Option[BoundedInterval[A]] =
      BoundedInterval.fromInterval(&(rhs))

    inline def intersect(rhs: BoundedInterval[A])(implicit o: Order[A]): Option[BoundedInterval[A]] =
      BoundedInterval.fromInterval(intersect(rhs))
    inline def intersect(rhs: Interval[A])(implicit o: Order[A]): Option[BoundedInterval[A]] =
      BoundedInterval.fromInterval(intersect(rhs))

    inline def unary_~(implicit o: Order[A]): List[Interval[A]] = self.unary_~

    inline def --(rhs: BoundedInterval[A])(implicit o: Order[A]): List[BoundedInterval[A]] =
      self.--(rhs).map(_.asInstanceOf[BoundedInterval[A]])
    inline def --(rhs: Interval[A])(implicit o: Order[A]): List[BoundedInterval[A]] =
      self.--(rhs).map(_.asInstanceOf[BoundedInterval[A]])

    inline def split(t: A)(implicit o: Order[A]): (Option[BoundedInterval[A]], Option[BoundedInterval[A]]) =
      val (a, b) = self.split(t)
      (BoundedInterval.fromInterval(a), BoundedInterval.fromInterval(b))

    inline def splitAtZero(implicit o: Order[A], ev: AdditiveMonoid[A]): (Option[BoundedInterval[A]], Option[BoundedInterval[A]]) =
      val (a, b) = self.splitAtZero
      (BoundedInterval.fromInterval(a), BoundedInterval.fromInterval(b))

    inline def mapAroundZero[B](f: Interval[A] => B)(implicit o: Order[A], ev: AdditiveMonoid[A]): (B, B) =
      self.mapAroundZero(f)

    inline def |(rhs: BoundedInterval[A])(implicit o: Order[A]): BoundedInterval[A] =
      self.|(rhs).asInstanceOf[BoundedInterval[A]]
    inline def |(rhs: Interval[A])(implicit o: Order[A]): BoundedInterval[A] =
      self.|(rhs).asInstanceOf[BoundedInterval[A]]

    inline def union(rhs: BoundedInterval[A])(implicit o: Order[A]): BoundedInterval[A] =
      self.union(rhs).asInstanceOf[BoundedInterval[A]]
    inline def union(rhs: Interval[A])(implicit o: Order[A]): BoundedInterval[A] =
      self.union(rhs).asInstanceOf[BoundedInterval[A]]

    /** True if this and the other interval abut each other. */
    def abuts(other: BoundedInterval[A])(using Eq[A]): Boolean =
      self.lower === other.upper || other.lower === self.upper

    /**
     * Join two abutting or overlapping intervals.
     *
     * This operation is only defined if the two intervals overlap or abut each other, i.e. in all
     * cases where adding the two intervals results in one single interval.
     */
    def join(other: BoundedInterval[A])(using Order[A]): Option[BoundedInterval[A]] =
      if (self.intersects(other) || abuts(other))
        self.union(other).asInstanceOf[BoundedInterval[A]].some
      else
        none
  }

  extension (self: IntervalSeq.type)
    def apply[T: Order](i: BoundedInterval[T]): IntervalSeq[T] =
      self(i.toInterval)

   /**
   * Makes a best-effort attempt to convert the tuple (a, b) into interval [a, b) or [b, a).
   *
   * Normalizes insto open upper intervals. Tuple values must be different.
   */
  def openUpperFromTuple[A: Order]: ValidSplitEpi[NonEmptyString, (A, A), BoundedInterval[A]] =
    ValidSplitEpi[NonEmptyString, (A, A), BoundedInterval[A]](
      { case (start, end) =>
        if (start < end)(BoundedInterval.unsafeOpenUpper(start, end)).asRight
        else if (start > end)(BoundedInterval.unsafeOpenUpper(end, start)).asRight
        else "Bounds must be different in order to build an open upper interval".refined[NonEmpty].asLeft
      },
      i => (i.lower, i.upper)
    )

  /**
   * Makes a best-effort attempt to convert the tuple (a, b) into interval [a, b] or [b, a].
   *
   * Normalizes both ways: closes intervals and sorts tuples.
   */
  def closedFromTuple[A: Order]: Wedge[(A, A), BoundedInterval[A]] =
    Wedge[(A, A), BoundedInterval[A]](
      { case (start, end) =>
        if (start <= end) BoundedInterval.unsafeClosed(start, end)
        else BoundedInterval.unsafeClosed(end, start)
      },
      i => (i.lower, i.upper)
    )

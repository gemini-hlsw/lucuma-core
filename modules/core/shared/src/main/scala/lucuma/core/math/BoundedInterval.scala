// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.syntax.all.*
import spire.math.Bounded
import spire.math.Empty
import spire.math.Interval
import spire.math.Point
import spire.math.interval.ValueBound

/**
  * A `BoundedInterval` is a `spire.math.Interval` but with both bounds defined.
  * Therefore, it can only be a `Bounded` or `Point`. It simplifies usage,
  * without having to worry about the `Empty`, `All`, `Above` and `Below` cases.
  */
type BoundedInterval[A] = Bounded[A] | Point[A]

object BoundedInterval:
  def closed[A: Order](lower: A, upper: A): Option[BoundedInterval[A]] = 
    Interval.closed(lower, upper).some.filterNot(_.isEmpty).map(_.asInstanceOf[BoundedInterval[A]])

  def unsafeClosed[A: Order](lower: A, upper: A): BoundedInterval[A] = 
    closed(lower, upper).getOrElse(sys.error(s"Could not build a bounded closed interval with lower bound [$lower] and upper bound [$upper]."))

  def openUpper[A: Order](lower: A, upper: A): Option[BoundedInterval[A]] = 
    Interval.openUpper(lower, upper).some.filterNot(_.isEmpty).map(_.asInstanceOf[BoundedInterval[A]])

  def unsafeOpenUpper[A: Order](lower: A, upper: A): BoundedInterval[A] =
    openUpper(lower, upper).getOrElse(sys.error(s"Could not build a bounded open upper interval with lower bound [$lower] and upper bound [$upper]."))

  given [A: Eq]: Eq[BoundedInterval[A]] = Interval.eq[A].contramap(identity)

  extension [A](self: BoundedInterval[A])
    def lowerValueBound: ValueBound[A] = self match
      case b @ Bounded(_, _, _) => b.lowerBound
      case p @ Point(_) => p.lowerBound
    
    def upperValueBound: ValueBound[A] = self match
         case b @ Bounded(_, _, _) => b.upperBound
         case p @ Point(_) => p.upperBound

    def lower: A = self match
      case b @ Bounded(_, _, _) => b.lower
      case p @ Point(_) => p.lower

    def upper: A = self match
      case b @ Bounded(_, _, _) => b.upper
      case p @ Point(_) => p.upper

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


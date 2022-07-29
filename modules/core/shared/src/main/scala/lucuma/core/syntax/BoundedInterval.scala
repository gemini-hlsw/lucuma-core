// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
package lucuma.core.syntax

import cats.Order
import cats.syntax.all._
import spire.math.Bounded
import spire.math.Interval
final class BoundedIntervalOps[A](private val self: Bounded[A]) extends AnyVal {

  /** True if this and the other interval abut each other. */
  def abuts(other: Bounded[A])(implicit order: Order[A]): Boolean =
    self.lower === other.upper || other.lower === self.upper

  /**
   * Join two abutting or overlapping intervals.
   *
   * This operation is only defined if the two intervals overlap or abut each other, i.e. in all
   * cases where adding the two intervals results in one single interval.
   */
  def join(other: Bounded[A])(implicit order: Order[A]): Option[Bounded[A]] =
    if (self.intersects(other) || abuts(other))
      self.union(other).asInstanceOf[Bounded[A]].some
    else
      none
}

final class BoundedModuleOps(private val self: Bounded.type) extends AnyVal {
  def unsafeOpenUpper[A: Order](lower: A, upper: A): Bounded[A] =
    Interval.openUpper(lower, upper).asInstanceOf[Bounded[A]]

  def unsafeClosed[A: Order](lower: A, upper: A): Bounded[A] =
    Interval.closed(lower, upper).asInstanceOf[Bounded[A]]
}

trait ToBoundedIntervalOps {
  implicit def ToBoundedIntervalOps[A](i: Bounded[A]): BoundedIntervalOps[A] =
    new BoundedIntervalOps(i)

  implicit def ToBoundedModuleOps(b: Bounded.type): BoundedModuleOps =
    new BoundedModuleOps(b)
}

object boundedInterval extends ToBoundedIntervalOps

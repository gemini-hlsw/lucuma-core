// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.instances

import cats.Eq
import spire.math.Bounded
import spire.math.Interval

trait BoundedIntervalInstances {
  implicit def BoundedIntervalEq[A: Eq]: Eq[Bounded[A]] =
    new Eq[Bounded[A]] {
      def eqv(x: Bounded[A], y: Bounded[A]): Boolean = Interval.eq[A].eqv(x, y)
    }

}

object boundedInterval extends BoundedIntervalInstances

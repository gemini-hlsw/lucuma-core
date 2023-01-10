// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.Eq
import cats.Monoid
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.api.Validate
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric.GreaterEqual
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.numeric.Less
import eu.timepit.refined.numeric.NonNegative
import org.typelevel.cats.time.instances.duration.*
import shapeless.Nat._0

import java.time.Duration
import java.time.temporal.Temporal

package object model {

  // Integer Percents
  type ZeroTo100  = Interval.Closed[0, 100]
  type IntPercent = Int Refined ZeroTo100

  object IntPercent extends RefinedTypeOps[IntPercent, Int]

  // Non negative duration
  given Plain[Duration, GreaterEqual[_0]] =
    Validate.fromPredicate(
      (d: Duration) => d.toNanos >= 0L,
      (d: Duration) => s"$d is not negative",
      Not(Less(shapeless.nat._0))
    )

  type NonNegDuration = Duration Refined NonNegative

  object NonNegDuration extends RefinedTypeOps[NonNegDuration, Duration] {

    val zero: NonNegDuration =
      unsafeFrom(Duration.ofNanos(0L))

    def between(startInclusive: Temporal, endExclusive: Temporal): NonNegDuration =
      NonNegDuration
        .unapply(Duration.between(startInclusive, endExclusive))
        .getOrElse(zero)
  }

  // You need to `import lucuma.core.model.given` to get these implicits
  given Eq[NonNegDuration] =
    Eq.instance { case (a, b) => a.value === b.value }

  given Monoid[NonNegDuration] =
    Monoid.instance(
      NonNegDuration.zero,
      (a, b) => NonNegDuration.unsafeFrom(a.value |+| b.value)
    )
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import cats.Order
import cats.Order.*
import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import spire.math.Bounded
import spire.math.Interval
import spire.math.extras.interval.IntervalSeq

trait ArbInterval:
  given xxx[A: Arbitrary: Order]: Arbitrary[BoundedInterval[A]] =
    Arbitrary(
      for 
        a <- arbitrary[A]
        b <- arbitrary[A].suchThat(_ =!= a)
      yield 
        val ab = List(a, b).sorted
        BoundedInterval.unsafeOpenUpper(ab(0), ab(1))
    )

  given [A: Cogen]: Cogen[Bounded[A]] =
    Cogen[(A, A)].contramap(i => (i.lower, i.upper))

  given [A: Arbitrary: Order]: Arbitrary[Interval[A]] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Interval.all[A]),
        Gen.const(Interval.empty[A]),
        arbitrary[A].map(a => Interval.above(a)),
        arbitrary[A].map(a => Interval.atOrAbove(a)),
        arbitrary[A].map(a => Interval.below(a)),
        arbitrary[A].map(a => Interval.atOrBelow(a)),
        arbitrary[A].map(a => Interval.point(a)),
        arbitrary[(A, A)].map { case (a, b) => Interval.closed(a, b) },
        arbitrary[(A, A)].map { case (a, b) => Interval.open(a, b) },
        arbitrary[(A, A)].map { case (a, b) => Interval.openLower(a, b) },
        arbitrary[(A, A)].map { case (a, b) => Interval.openUpper(a, b) }
      )
    )

  given [A: Arbitrary: Order]: Arbitrary[IntervalSeq[A]] =
    Arbitrary(arbitrary[List[Interval[A]]].map(_.foldLeft(IntervalSeq.empty[A])((s, i) => s | i)))

object ArbInterval extends ArbInterval

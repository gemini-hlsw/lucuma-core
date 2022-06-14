// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation.arb

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.numeric.intervalClosedArbitrary
import lucuma.core.validation.TruncatedRefinedBigDecimal
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbTruncatedRefinedBigDecimal {
  val OneBD   = BigDecimal(1.0)
  val ThreeBD = BigDecimal(3.0)
  type OneToThree    = Interval.Closed[OneBD.type, ThreeBD.type]
  type BigOneToThree = BigDecimal Refined OneToThree

  implicit val arbClosed: Arbitrary[BigOneToThree] = intervalClosedArbitrary

  implicit val arbTruncRefinedBD = Arbitrary[TruncatedRefinedBigDecimal[OneToThree, 1]] {
    arbitrary[BigOneToThree].map(TruncatedRefinedBigDecimal[OneToThree, 1](_).get)
  }

  implicit def cogTruncRefinedBD: Cogen[TruncatedRefinedBigDecimal[OneToThree, 1]] =
    Cogen[BigOneToThree].contramap(trbd => trbd.value)
}

object ArbTruncatedRefinedBigDecimal extends ArbTruncatedRefinedBigDecimal

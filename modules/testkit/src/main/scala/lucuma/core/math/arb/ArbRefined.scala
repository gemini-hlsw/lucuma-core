// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.refineV
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.auto._
import lucuma.core.math.refined._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbRefined {
  val BigDecimalZero: BigDecimal      = BigDecimal(0)
  val PosBigDecimalOne: PosBigDecimal = BigDecimal(1).refined

  given Arbitrary[NonNegInt] = Arbitrary(Gen.choose(0, Int.MaxValue).map(refineV[NonNegative](_).toOption.get))
  given Cogen[NonNegInt] = Cogen[Int].contramap(x => x)

  // Refined does not derive this arbitrary. Generally deriving `Arbitrary` instances for `Pos`
  // numbers requires instances of `Adjacent` and `Max`, which don't exist for `BigDecimal`.
  implicit val arbPosBigDecimal: Arbitrary[PosBigDecimal] =
    Arbitrary(
      arbitrary[BigDecimal]
        .map {
          case BigDecimalZero => PosBigDecimalOne
          case x              => PosBigDecimal.unsafeFrom(x.abs)
        }
    )

  implicit def cogenRefined[A: Cogen, P]: Cogen[A Refined P] =
    Cogen[A].contramap(_.value)
}

object ArbRefined extends ArbRefined

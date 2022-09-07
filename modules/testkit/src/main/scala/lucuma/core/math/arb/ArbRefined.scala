// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.refined._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbRefined {
  val BigDecimalZero: BigDecimal      = BigDecimal(0)
  val PosBigDecimalOne: PosBigDecimal = BigDecimal(1).refined

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

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.types.numeric.PosBigDecimal
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbRefined {
  val BigDecimalZero: BigDecimal      = BigDecimal(0)
  val PosBigDecimalOne: PosBigDecimal = PosBigDecimal(BigDecimal(1))

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

  implicit val cogenPosBigDecimal: Cogen[PosBigDecimal] =
    Cogen[BigDecimal].contramap(_.value)
}

object ArbRefined extends ArbRefined

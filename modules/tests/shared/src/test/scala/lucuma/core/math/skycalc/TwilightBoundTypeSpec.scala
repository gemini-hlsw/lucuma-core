// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.math.skycalc.TwilightBoundType

final class TwilightBoundTypeSpec extends CatsSuite {
  import ArbTwilightBoundType._

  // Laws
  checkAll("TwilightBoundType", EqTests[TwilightBoundType].eqv)
  checkAll("TwilightBoundType",
           OrderTests[TwilightBoundType](TwilightBoundType.TwilightBoundTypeOrder).order
  )

  test("Equality must be natural") {
    forAll { (a: TwilightBoundType, b: TwilightBoundType) =>
      a.equals(b) shouldEqual Eq[TwilightBoundType].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: TwilightBoundType) =>
      a.toString shouldEqual Show[TwilightBoundType].show(a)
    }
  }
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import gsp.math.arb._
import gsp.math.skycalc.TwilightBoundType

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
object TwilightBoundTypeSpec extends CatsSuite {
  import ArbTwilightBoundType._

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

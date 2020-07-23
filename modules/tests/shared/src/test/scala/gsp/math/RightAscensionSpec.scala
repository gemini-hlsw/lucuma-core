// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Order, Show }
import cats.kernel.laws.discipline._
import gsp.math.arb._
import gsp.math.laws.discipline._
import monocle.law.discipline._

final class RightAscensionSpec extends CatsSuite {
  import ArbRightAscension._
  import ArbAngle._

  // Laws
  checkAll("RightAscension", OrderTests[RightAscension].order)
  checkAll("fromAngleExact", PrismTests(RightAscension.fromAngleExact))
  checkAll("fromHourAngle", IsoTests(RightAscension.fromHourAngle))
  checkAll("fromStringHMS",
           FormatTests(RightAscension.fromStringHMS).formatWith(ArbAngle.stringsHMS)
  )

  test("Equality must be natural") {
    forAll { (a: RightAscension, b: RightAscension) =>
      a.equals(b) shouldEqual Eq[RightAscension].eqv(a, b)
    }
  }

  test("Order must be consistent with .toHourAngle.toMicroarcseconds") {
    forAll { (a: RightAscension, b: RightAscension) =>
      Order[Long].comparison(a.toHourAngle.toMicroarcseconds,
                             b.toHourAngle.toMicroarcseconds
      ) shouldEqual
        Order[RightAscension].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: RightAscension) =>
      a.toString shouldEqual Show[RightAscension].show(a)
    }
  }

}

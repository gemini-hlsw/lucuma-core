// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.{ Eq, Order, Show }
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import monocle.law.discipline._
import org.scalacheck.Prop._

final class RightAscensionSuite extends munit.DisciplineSuite {
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
      assertEquals(a.equals(b), Eq[RightAscension].eqv(a, b))
    }
  }

  test("Order must be consistent with .toHourAngle.toMicroarcseconds") {
    forAll { (a: RightAscension, b: RightAscension) =>
      assertEquals(Order[Long].comparison(a.toHourAngle.toMicroarcseconds,
                             b.toHourAngle.toMicroarcseconds),
        Order[RightAscension].comparison(a, b)
      )
    }
  }

  test("Show must be natural") {
    forAll { (a: RightAscension) =>
      assertEquals(a.toString, Show[RightAscension].show(a))
    }
  }

}

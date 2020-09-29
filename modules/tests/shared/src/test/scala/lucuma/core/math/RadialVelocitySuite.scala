// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import lucuma.core.math.arb._
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Prop._

final class RadialVelocitySuite extends DisciplineSuite {
  import ArbRadialVelocity._

  // Laws
  checkAll("fromMetersPerSecond", PrismTests(RadialVelocity.fromMetersPerSecond))

  test("Equality must be natural") {
    forAll { (a: RadialVelocity, b: RadialVelocity) =>
      assertEquals(a.equals(b), Eq[RadialVelocity].eqv(a, b))
    }
  }

  test("Order must be consistent with .rv") {
    forAll { (a: RadialVelocity, b: RadialVelocity) =>
      assertEquals(Order[BigDecimal].comparison(a.rv.value, b.rv.value),
                   Order[RadialVelocity].comparison(a, b)
      )
    }
  }

  test("Show must be natural") {
    forAll { (a: RadialVelocity) =>
      assertEquals(a.toString, Show[RadialVelocity].show(a))
    }
  }

}

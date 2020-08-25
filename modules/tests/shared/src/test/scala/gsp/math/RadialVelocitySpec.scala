// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.math

import cats._
import cats.tests.CatsSuite
import lucuma.math.arb._
import monocle.law.discipline._

final class RadialVelocitySpec extends CatsSuite {
  import ArbRadialVelocity._

  // Laws
  checkAll("fromMetersPerSecond", PrismTests(RadialVelocity.fromMetersPerSecond))

  test("Equality must be natural") {
    forAll { (a: RadialVelocity, b: RadialVelocity) =>
      a.equals(b) shouldEqual Eq[RadialVelocity].eqv(a, b)
    }
  }

  test("Order must be consistent with .rv") {
    forAll { (a: RadialVelocity, b: RadialVelocity) =>
      Order[BigDecimal].comparison(a.rv.value, b.rv.value) shouldEqual
        Order[RadialVelocity].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: RadialVelocity) =>
      a.toString shouldEqual Show[RadialVelocity].show(a)
    }
  }

}


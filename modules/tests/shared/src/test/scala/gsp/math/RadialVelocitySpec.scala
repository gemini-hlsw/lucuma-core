// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats._
import cats.tests.CatsSuite
import gsp.math.laws.discipline._
import gsp.math.arb._
import monocle.law.discipline._

final class RadialVelocitySpec extends CatsSuite {
  import ArbRadialVelocity._

  // Laws
  checkAll("fromMetersPerSecond", PrismTests(RadialVelocity.fromMetersPerSecond))
  checkAll("fromKilometersPerSecond", FormatTests(RadialVelocity.fromKilometersPerSecond).format)

  test("Equality must be natural") {
    forAll { (a: RadialVelocity, b: RadialVelocity) =>
      a.equals(b) shouldEqual Eq[RadialVelocity].eqv(a, b)
    }
  }

  test("Order must be consistent with .toMetersPerSecond") {
    forAll { (a: RadialVelocity, b: RadialVelocity) =>
      Order[Int].comparison(a.toMetersPerSecond.value, b.toMetersPerSecond.value) shouldEqual
        Order[RadialVelocity].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: RadialVelocity) =>
      a.toString shouldEqual Show[RadialVelocity].show(a)
    }
  }

}


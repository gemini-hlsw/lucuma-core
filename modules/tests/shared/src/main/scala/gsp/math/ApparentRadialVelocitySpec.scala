// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import java.math.MathContext

import cats._
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import coulomb._
import coulomb.si._
import coulomb.siprefix._
import gsp.math.arb._
import gsp.math.units._

final class ApparentRadialVelocitySpec extends CatsSuite {
  import ArbApparentRadialVelocity._

  // Laws
  checkAll("ApparentRadialVelocity", EqTests[ApparentRadialVelocity].eqv)
  checkAll("ApparentRadialVelocityOrder", OrderTests[ApparentRadialVelocity].order)

  test("toRedshift") {
    assert(
      // Note the speed is given in Meter per second but coulomb will convert
      ApparentRadialVelocity(0.withUnit[MetersPerSecond]).toRedshift === Redshift.Zero
    )
    assert(ApparentRadialVelocity(RadialVelocity.C).toRedshift === Redshift(1))
    assert(
      ApparentRadialVelocity(
        BigDecimal.decimal(1000, MathContext.DECIMAL32).withUnit[KilometersPerSecond]
      ).toRedshift === Redshift(BigDecimal.decimal(0.003335641, MathContext.DECIMAL32))
    )
  }

  test("Equality must be natural") {
    forAll { (a: ApparentRadialVelocity, b: ApparentRadialVelocity) =>
      a.equals(b) shouldEqual Eq[ApparentRadialVelocity].eqv(a, b)
    }
  }

  test("Order must be consistent with .cz") {
    forAll { (a: ApparentRadialVelocity, b: ApparentRadialVelocity) =>
      Order[BigDecimal].comparison(a.cz.value, b.cz.value) shouldEqual
        Order[ApparentRadialVelocity].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: ApparentRadialVelocity) =>
      a.toString shouldEqual Show[ApparentRadialVelocity].show(a)
    }
  }
}

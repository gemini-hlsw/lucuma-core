// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import java.math.MathContext

import cats._
import cats.kernel.laws.discipline._
import cats.tests.CatsSuite
import coulomb._
import coulomb.si._
import lucuma.core.math.arb._
import lucuma.core.math.units._

final class RedshiftSpec extends CatsSuite {
  import ArbRedshift._

  // Laws
  checkAll("Redshift", EqTests[Redshift].eqv)
  checkAll("RedshiftOrder", OrderTests[Redshift].order)

  test("toRadialVelocity") {
    assert(Redshift.Zero.toRadialVelocity === RadialVelocity(0.withUnit[MetersPerSecond]))
    assert(
      // Example from http://spiff.rit.edu/classes/phys240/lectures/expand/expand.html
      // We need to specify the Math context to properly compare
      Redshift(BigDecimal.decimal(5.82, MathContext.DECIMAL64)).toRadialVelocity === RadialVelocity(
        BigDecimal
          .decimal(287172.9120288430, MathContext.DECIMAL64)
          .withUnit[KilometersPerSecond]
      )
    )
  }

  test("toApparentRadialVelocity") {
    assert(
      Redshift.Zero.toApparentRadialVelocity === ApparentRadialVelocity(0.withUnit[MetersPerSecond])
    )
    assert(Redshift(1).toApparentRadialVelocity === ApparentRadialVelocity(RadialVelocity.C))
    assert(
      // In apparent radial velocity we can go faster than C
      Redshift(
        BigDecimal.decimal(5.82, MathContext.DECIMAL64)
      ).toApparentRadialVelocity === ApparentRadialVelocity(
        BigDecimal
          .decimal(1744792.10556, MathContext.DECIMAL64)
          .withUnit[KilometersPerSecond]
      )
    )
  }

  test("Equality must be natural") {
    forAll { (a: Redshift, b: Redshift) =>
      a.equals(b) shouldEqual Eq[Redshift].eqv(a, b)
    }
  }

  test("Order must be consistent with .z") {
    forAll { (a: Redshift, b: Redshift) =>
      Order[BigDecimal].comparison(a.z, b.z) shouldEqual
        Order[Redshift].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Redshift) =>
      a.toString shouldEqual Show[Redshift].show(a)
    }
  }
}

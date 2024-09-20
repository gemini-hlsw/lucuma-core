// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.*
import cats.kernel.laws.discipline.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import lucuma.core.math.Constants.SpeedOfLight
import lucuma.core.math.arb.*
import lucuma.core.math.units.*
import monocle.law.discipline.IsoTests
import org.scalacheck.Prop.*

import java.math.MathContext

final class RedshiftSuite extends munit.DisciplineSuite {
  import ArbRedshift.given

  // Laws
  checkAll("Redshift", EqTests[Redshift].eqv)
  checkAll("RedshiftOrder", OrderTests[Redshift].order)
  checkAll("redshift", IsoTests(Redshift.redshift))

  test("toRadialVelocity") {
    assertEquals(Redshift.Zero.toRadialVelocity, RadialVelocity((BigDecimal(0).withUnit[MetersPerSecond])))
    assertEquals(
      // Example from http://spiff.rit.edu/classes/phys240/lectures/expand/expand.html
      // We need to specify the Math context to properly compare
      Redshift(BigDecimal.decimal(5.82, MathContext.DECIMAL64)).toRadialVelocity,
      RadialVelocity(
        BigDecimal
          .decimal(287172.9120288430, MathContext.DECIMAL64)
          .withUnit[KilometersPerSecond]
          .toUnit[MetersPerSecond]
      )
    )
  }

  test("toApparentRadialVelocity") {
    assertEquals(
      Redshift.Zero.toApparentRadialVelocity,
      ApparentRadialVelocity(BigDecimal(0).withUnit[MetersPerSecond])
    )
    assertEquals(Redshift(1).toApparentRadialVelocity, ApparentRadialVelocity(SpeedOfLight.toValue[BigDecimal]))
    assertEquals(
      // In apparent radial velocity we can go faster than C
      Redshift(
        BigDecimal.decimal(5.82, MathContext.DECIMAL64)
      ).toApparentRadialVelocity,
      ApparentRadialVelocity(
        BigDecimal
          .decimal(1744792.10556, MathContext.DECIMAL64)
          .withUnit[KilometersPerSecond]
          .toUnit[MetersPerSecond]
      )
    )
  }

  test("Equality must be natural") {
    forAll { (a: Redshift, b: Redshift) =>
      assertEquals(a.equals(b), Eq[Redshift].eqv(a, b))
    }
  }

  test("Order must be consistent with .z") {
    forAll { (a: Redshift, b: Redshift) =>
      assertEquals(Order[BigDecimal].comparison(a.z, b.z), Order[Redshift].comparison(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Redshift) =>
      assertEquals(a.toString, Show[Redshift].show(a))
    }
  }
}

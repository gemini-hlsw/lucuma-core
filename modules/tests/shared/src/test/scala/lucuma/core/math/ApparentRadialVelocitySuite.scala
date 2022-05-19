// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import cats.kernel.laws.discipline._
import coulomb._
import coulomb.units.si._
import lucuma.core.math.Constants.SpeedOfLight
import lucuma.core.math.arb._
import lucuma.core.math.units._
import lucuma.core.optics.laws.discipline.WedgeTests
import monocle.law.discipline.IsoTests
import org.scalacheck.Prop._

import java.math.MathContext

final class ApparentRadialVelocitySuite extends munit.DisciplineSuite {
  import ArbApparentRadialVelocity._

  // Laws
  checkAll("ApparentRadialVelocity", EqTests[ApparentRadialVelocity].eqv)
  checkAll("ApparentRadialVelocityOrder", OrderTests[ApparentRadialVelocity].order)
  checkAll("meterspersecond", IsoTests(ApparentRadialVelocity.meterspersecond))
  checkAll("kilometerspersecond", WedgeTests(ApparentRadialVelocity.kilometerspersecond).wedge)

  test("toRedshift") {
    assertEquals(
      // Note the speed is given in Meter per second but coulomb will convert
      ApparentRadialVelocity(0.withUnit[MetersPerSecond]).toRedshift,
      Redshift.Zero
    )
    assertEquals(ApparentRadialVelocity(SpeedOfLight).toRedshift, Redshift(1))
    assertEquals(
      ApparentRadialVelocity(
        BigDecimal.decimal(1744792.10556, MathContext.DECIMAL64).withUnit[KilometersPerSecond]
      ).toRedshift,
      Redshift(BigDecimal.decimal(5.82, MathContext.DECIMAL64))
    )
  }

  test("Equality must be natural") {
    forAll { (a: ApparentRadialVelocity, b: ApparentRadialVelocity) =>
      assertEquals(a.equals(b), Eq[ApparentRadialVelocity].eqv(a, b))
    }
  }

  test("Order must be consistent with .cz") {
    forAll { (a: ApparentRadialVelocity, b: ApparentRadialVelocity) =>
      assertEquals(Order[BigDecimal].comparison(a.cz.value, b.cz.value),
                   Order[ApparentRadialVelocity].comparison(a, b)
      )
    }
  }

  test("Show must be natural") {
    forAll { (a: ApparentRadialVelocity) =>
      assertEquals(a.toString, Show[ApparentRadialVelocity].show(a))
    }
  }
}

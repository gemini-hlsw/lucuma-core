// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.syntax.all._
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import monocle.law.discipline._
import org.scalacheck.Prop._

final class DeclinationSuite extends munit.DisciplineSuite {
  import ArbDeclination._
  import ArbAngle._

  // Laws
  checkAll("Declination", OrderTests[Declination].order)
  checkAll("fromAngle", PrismTests(Declination.fromAngle))
  checkAll("fromStringHMS",
           FormatTests(Declination.fromStringSignedDMS).formatWith(ArbAngle.stringsDMS)
  )

  test("Equality must be natural") {
    forAll { (a: Declination, b: Declination) =>
      assertEquals(a.equals(b), Eq[Declination].eqv(a, b))
    }
  }

  test("Eq must be consistent with .toAngle.toMicroarcseconds") {
    forAll { (a: Declination, b: Declination) =>
      assertEquals(Eq[Long].eqv(a.toAngle.toMicroarcseconds, b.toAngle.toMicroarcseconds),
        Eq[Declination].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Declination) =>
      assertEquals(a.toString, Show[Declination].show(a))
    }
  }

  test("Construction must be consistent between fromAngle and fromAngleWithCarry") {
    forAll { (a: Angle) =>
      (Declination.fromAngle.getOption(a), Declination.fromAngleWithCarry(a)) match {
        case (Some(d), (dʹ, false)) => assertEquals(d, dʹ)
        case (None, (d, true))      => assertEquals(d.toAngle, a.mirrorBy(Angle.Angle90))
        case _                      => fail("Unpossible")
      }
    }
  }

  test("Offsetting must have an identity") {
    forAll { (a: Declination) =>
      assertEquals(a.offset(Angle.Angle0), (a, false))
    }
  }

  test("Offsetting must be invertible") {
    forAll { (a: Declination, b: Angle) =>
      a.offset(b) match {
        case (aʹ, false) => assertEquals(aʹ.offset(-b), (a, false))
        case (aʹ, true)  => assertEquals(aʹ.offset(b), (a, true))
      }
    }
  }

  test("Declination from degrees") {
    // Sanity checks
    assertEquals(Declination.fromDoubleDegrees(0), Declination.Zero.some)
    assertEquals(Declination.fromDoubleDegrees(90), Declination.Max.some)
    assertEquals(Declination.fromDoubleDegrees(-90), Declination.Min.some)
    assertEquals(Declination.fromDoubleDegrees(-90), Declination.fromDoubleDegrees(270))
    assertEquals(Declination.fromDoubleDegrees(180), None)
  }

}

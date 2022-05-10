// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.Show
import cats.kernel.laws.discipline._
import cats.syntax.all._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import monocle.law.discipline._
import org.scalacheck.Prop._

final class AngleSuite extends munit.DisciplineSuite {
  import ArbAngle._

  // Laws
  checkAll("Angle", CommutativeGroupTests[Angle].commutativeGroup)
  checkAll("Angle", EqTests[Angle].eqv)
  checkAll("Angle", OrderTests[Angle](Angle.AngleOrder).order)
  checkAll("SignedAngle", OrderTests[Angle](Angle.SignedAngleOrder).order)

  // Optics
  checkAll("microarcseconds", SplitMonoTests(Angle.microarcseconds).splitMono)
  checkAll("signedMicroarcseconds", SplitMonoTests(Angle.signedMicroarcseconds).splitMono)
  checkAll("signedDecimalMilliarcseconds",
           SplitMonoTests(Angle.signedDecimalMilliarcseconds).splitMono
  )
  checkAll("signedDecimalArcseconds", SplitMonoTests(Angle.signedDecimalArcseconds).splitMono)
  checkAll("milliarcseconds", WedgeTests(Angle.milliarcseconds).wedge)
  checkAll("arcseconds", WedgeTests(Angle.arcseconds).wedge)
  checkAll("arcminutes", WedgeTests(Angle.arcminutes).wedge)
  checkAll("degrees", WedgeTests(Angle.degrees).wedge)
  checkAll("hourAngle", SplitEpiTests(Angle.hourAngle).splitEpi)
  checkAll("hourAngleExact", PrismTests(Angle.hourAngleExact))
  checkAll("dms", IsoTests(Angle.dms))
  checkAll("fromStringDMS", FormatTests(Angle.fromStringDMS).formatWith(ArbAngle.stringsDMS))
  checkAll("fromStringSignedDMS",
           FormatTests(Angle.fromStringSignedDMS).formatWith(ArbAngle.stringsSignedDMS)
  )

  test("Equality must be natural") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(a.equals(b), Eq[Angle].eqv(a, b))
    }
  }

  test("Equality must be consistent with .toMicroarcseconds") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(Eq[Long].eqv(a.toMicroarcseconds, b.toMicroarcseconds),
        Eq[Angle].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Angle) =>
      assertEquals(a.toString, Show[Angle].show(a))
    }
  }

  // N.B. this is *not* covered by the `dms` Iso test.
  test("Conversion to DMS must be invertable") {
    forAll { (a: Angle) =>
      val dms = Angle.dms.get(a)
      assertEquals(Angle.fromDMS(
        dms.degrees,
        dms.arcminutes,
        dms.arcseconds,
        dms.milliarcseconds,
        dms.microarcseconds
      ), a)
    }
  }

  test("Flipping must be invertable") {
    forAll { (a: Angle) =>
      assertEquals(a.flip.flip, a)
    }
  }

  test("Construction must normalize [non-pathological] angles") {
    forAll { (a: Angle, n: Int) =>
      val factor   = n % 10
      val masIn360 = 360L * 60L * 60L * 1000L * 1000L
      val offset   = masIn360 * factor
      val b        = Angle.fromMicroarcseconds(a.toMicroarcseconds + offset)
      assertEquals(a, b)
    }
  }

  // In principle I think this is the only thing we need to check.
  test("mirrorBy must obey the mirror law") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(b - a, (a.mirrorBy(b)) - b)
    }
  }

  test("mirrorBy must be reflexive") {
    forAll { (a: Angle) =>
      assertEquals(a.mirrorBy(a), a)
    }
  }

  test("mirrorBy must be invertible") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(a.mirrorBy(b).mirrorBy(b), a)
    }
  }

  test("mirrorBy must be invariant to flip in mirror angle") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(a.mirrorBy(b), a.mirrorBy(b.flip))
    }
  }

  test("mirrorBy must distribute over flip in target angle") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(a.mirrorBy(b).flip, a.flip.mirrorBy(b))
    }
  }

  test("mirrorBy must be consistent with flip") {
    forAll { (a: Angle) =>
      assertEquals(a.mirrorBy(a + Angle.Angle90), a.flip)
    }
  }

  test("mirrorBy must be consistent with unary negation") {
    forAll { (a: Angle) =>
      assertEquals(a.mirrorBy(Angle.Angle0), -a)
    }
  }

  test("HourAngle should (almost) round-trip double hours") {
    forAll { (a: HourAngle) =>
      val hrs  = a.toDoubleHours
      val hrsʹ = HourAngle.fromDoubleHours(hrs).toDoubleHours
      assertEqualsDouble(hrs, hrsʹ, 0.000000001)
    }
  }

  test("Difference with itself is 0") {
    forAll { (a: Angle) =>
      assertEquals(a.difference(a), Angle.Angle0)
    }
  }

  test("Difference with reverse is 180") {
    forAll { (a: Angle) =>
      assertEquals(a.difference(a + Angle.Angle180), Angle.Angle180)
    }
  }

  test("Difference is in the range [0 .. π]") {
    implicit val order: Order[Angle] = Angle.AngleOrder
    forAll { (a: Angle, b: Angle) =>
      assert(a.difference(b) >= Angle.Angle0 && a.difference(b) <= Angle.Angle180)
    }
  }

  test("Difference is commutative") {
    forAll { (a: Angle, b: Angle) =>
      assertEquals(a.difference(b), b.difference(a))
    }
  }

  test("A bisected Angle has half the magnitude") {
    forAll { (a: Angle) =>
      val b = (a + a).bisect
      assert(a === b || a.flip === b)
    }
  }

  test("Trigonometric functions") {
    forAll { (a: Angle) =>
      // Trivial checks
      assertEquals(a.sin, scala.math.sin(a.toDoubleRadians))
      assertEquals(a.cos, scala.math.cos(a.toDoubleRadians))
      assertEquals(a.tan, scala.math.tan(a.toDoubleRadians))
    }
  }
}

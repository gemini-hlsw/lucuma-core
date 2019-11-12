// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Show, Order }
import cats.kernel.laws.discipline._
import gsp.math.arb._
import gsp.math.laws.discipline._
import monocle.law.discipline._

final class AngleSpec extends CatsSuite {
  import ArbAngle._

  // Laws
  checkAll("Angle", CommutativeGroupTests[Angle].commutativeGroup)
  checkAll("Angle", EqTests[Angle].eqv)
  checkAll("Angle", OrderTests[Angle](Angle.AngleOrder).order)
  checkAll("SignedAngle", OrderTests[Angle](Angle.SignedAngleOrder).order)

  // Optics
  checkAll("microarcseconds", SplitMonoTests(Angle.microarcseconds).splitMono)
  checkAll("signedMicroarcseconds", SplitMonoTests(Angle.signedMicroarcseconds).splitMono)
  checkAll("signedMilliarcseconds", SplitMonoTests(Angle.signedMilliarcseconds).splitMono)
  checkAll("signedArcseconds", SplitMonoTests(Angle.signedArcseconds).splitMono)
  checkAll("milliarcseconds", WedgeTests(Angle.milliarcseconds).wedge)
  checkAll("arcseconds", WedgeTests(Angle.arcseconds).wedge)
  checkAll("arcminutes", WedgeTests(Angle.arcminutes).wedge)
  checkAll("degrees", WedgeTests(Angle.degrees).wedge)
  checkAll("hourAngle", SplitEpiTests(Angle.hourAngle).splitEpi)
  checkAll("hourAngleExact", PrismTests(Angle.hourAngleExact))
  checkAll("dms", IsoTests(Angle.dms))
  checkAll("fromStringDMS", FormatTests(Angle.fromStringDMS).formatWith(ArbAngle.stringsDMS))
  checkAll("fromStringSignedDMS", FormatTests(Angle.fromStringSignedDMS).formatWith(ArbAngle.stringsSignedDMS))

  test("Equality must be natural") {
    forAll { (a: Angle, b: Angle) =>
      a.equals(b) shouldEqual Eq[Angle].eqv(a, b)
    }
  }

  test("Equality must be consistent with .toMicroarcseconds") {
    forAll { (a: Angle, b: Angle) =>
      Eq[Long].eqv(a.toMicroarcseconds, b.toMicroarcseconds) shouldEqual
      Eq[Angle].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Angle) =>
      a.toString shouldEqual Show[Angle].show(a)
    }
  }

  // N.B. this is *not* covered by the `dms` Iso test.
  test("Conversion to DMS must be invertable") {
    forAll { (a: Angle) =>
      val dms = Angle.dms.get(a)
      Angle.fromDMS(
        dms.degrees,
        dms.arcminutes,
        dms.arcseconds,
        dms.milliarcseconds,
        dms.microarcseconds
      ) shouldEqual a
    }
  }

  test("Flipping must be invertable") {
    forAll { (a: Angle) =>
      a.flip.flip shouldEqual a
    }
  }

  test("Construction must normalize [non-pathological] angles") {
    forAll { (a: Angle, n: Int) =>
      val factor   = n % 10
      val masIn360 = 360L * 60L * 60L * 1000L * 1000L
      val offset   = masIn360 * factor
      val b = Angle.fromMicroarcseconds(a.toMicroarcseconds + offset)
      a shouldEqual b
    }
  }

  // In principle I think this is the only thing we need to check.
  test("mirrorBy must obey the mirror law") {
    forAll { (a: Angle, b: Angle) =>
      b - a shouldEqual (a mirrorBy b) - b
    }
  }

  test("mirrorBy must be reflexive") {
    forAll { (a: Angle) =>
      a mirrorBy a shouldEqual a
    }
  }

  test("mirrorBy must be invertible") {
    forAll { (a: Angle, b: Angle) =>
      a.mirrorBy(b).mirrorBy(b) shouldEqual a
    }
  }

  test("mirrorBy must be invariant to flip in mirror angle") {
    forAll { (a: Angle, b: Angle) =>
      a.mirrorBy(b) shouldEqual a.mirrorBy(b.flip)
    }
  }

  test("mirrorBy must distribute over flip in target angle") {
    forAll { (a: Angle, b: Angle) =>
      (a mirrorBy b).flip shouldEqual (a.flip mirrorBy b)
    }
  }

  test("mirrorBy must be consistent with flip") {
    forAll { (a: Angle) =>
      a.mirrorBy(a + Angle.Angle90) shouldEqual a.flip
    }
  }

  test("mirrorBy must be consistent with unary negation") {
    forAll { (a: Angle) =>
      a.mirrorBy(Angle.Angle0) shouldEqual -a
    }
  }

  test("HourAngle should (almost) round-trip double hours") {
    forAll { (a: HourAngle) =>
      val hrs  = a.toDoubleHours
      val hrsʹ = HourAngle.fromDoubleHours(hrs).toDoubleHours
      hrs shouldEqual hrsʹ +- 0.000000001
    }
  }

  test("Difference with itself is 0") {
    forAll { (a: Angle) =>
      a.difference(a) shouldEqual Angle.Angle0
    }
  }

  test("Difference with reverse is 180") {
    forAll { (a: Angle) =>
      a.difference(a + Angle.Angle180) shouldEqual Angle.Angle180
    }
  }

  test("Difference is in the range [0 .. π]") {
    implicit val order: Order[Angle] = Angle.AngleOrder
    forAll { (a: Angle, b: Angle) =>
      (a.difference(b) >= Angle.Angle0 && a.difference(b) <= Angle.Angle180) shouldBe true
    }
  }

  test("Difference is commutative") {
    forAll { (a: Angle, b: Angle) =>
      a.difference(b) shouldEqual b.difference(a)
    }
  }

}

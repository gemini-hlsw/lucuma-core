// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import monocle.law.discipline._
import org.scalacheck.Prop._

final class HourAngleSuite extends munit.DisciplineSuite {
  import ArbAngle._

  // Laws
  checkAll("HourAngle", CommutativeGroupTests[HourAngle].commutativeGroup)
  checkAll("HourAngle", EqTests[HourAngle].eqv)
  checkAll("HourAngle <: Angle", SubgroupTests[HourAngle, Angle].subgroup)

  // Optics
  checkAll("angle", SplitMonoTests(HourAngle.angle).splitMono)
  checkAll("microseconds", SplitMonoTests(HourAngle.microseconds).splitMono)
  checkAll("milliseconds", WedgeTests(HourAngle.milliseconds).wedge)
  checkAll("seconds", WedgeTests(HourAngle.seconds).wedge)
  checkAll("minutes", WedgeTests(HourAngle.minutes).wedge)
  checkAll("hours", WedgeTests(HourAngle.hours).wedge)
  checkAll("hms", IsoTests(HourAngle.hms))
  checkAll("fromStringHMS", FormatTests(HourAngle.fromStringHMS).formatWith(ArbAngle.stringsHMS))

  test("Equality must be natural") {
    forAll { (a: HourAngle, b: HourAngle) =>
      assertEquals(a.equals(b),  Eq[HourAngle].eqv(a, b))
    }
  }

  test("Equality must be consistent with .toMicroseconds") {
    forAll { (a: HourAngle, b: HourAngle) =>
      assertEquals(Eq[Long].eqv(a.toMicroseconds, b.toMicroseconds),
        Eq[HourAngle].eqv(a, b)
      )
    }
  }

  test("Show must be natural") {
    forAll { (a: HourAngle) =>
      assertEquals(a.toString,  Show[HourAngle].show(a))
    }
  }

  test("Conversion to HMS must be invertable") {
    forAll { (a: HourAngle) =>
      val hms = HourAngle.hms.get(a)
      assertEquals(HourAngle.fromHMS(
        hms.hours,
        hms.minutes,
        hms.seconds,
        hms.milliseconds,
        hms.microseconds),
        a)
    }
  }

  test("Flipping must be invertable") {
    forAll { (a: HourAngle) =>
      assertEquals(a.flip.flip,  a)
    }
  }

  test("Construction must normalize [non-pathological] angles") {
    forAll { (a: HourAngle, n: Int) =>
      val factor = n % 10
      val msIn24 = 24L * 60L * 60L * 1000L * 1000L
      val offset = msIn24 * factor
      val b      = HourAngle.fromMicroseconds(a.toMicroseconds + offset)
      assertEquals(a,  b)
    }
  }

}

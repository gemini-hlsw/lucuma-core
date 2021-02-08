// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import lucuma.core.model.arb._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import munit._
import org.scalacheck.Prop._

final class ElevationRangeSuite extends DisciplineSuite {
  import ArbElevationRange._

  // Laws
  checkAll("Eq[AirmassRange]", EqTests[AirmassRange].eqv)
  checkAll("AirmassRange.fromDeciVals", SplitEpiTests(AirmassRange.fromDeciVals).splitEpi)
  checkAll("Eq[HourAngleRange", EqTests[HourAngleRange].eqv)
  checkAll("HourAngleRange.fromDeciHours", SplitEpiTests(HourAngleRange.fromDeciHours).splitEpi)
  checkAll("Eq[ElevationRange]", EqTests[ElevationRange].eqv)

  test("AirmassRange.apply") {
    forAll { range: AirmassRange =>
      assert(range.deciMin.value <= range.deciMax.value)
    }
  }

  test("AirmassRange.setMin") {
    forAll { (min: AirmassRange.IntDeciValue, range: AirmassRange) =>
      val newRange = range.setMin(min)
      assertEquals(min, newRange.deciMin)
      assert(newRange.deciMin.value <= newRange.deciMax.value)
    }
  }

  test("AirmassRange.setMax") {
    forAll { (max: AirmassRange.IntDeciValue, range: AirmassRange) =>
      val newRange = range.setMax(max)
      assertEquals(max, newRange.deciMax)
      assert(newRange.deciMin.value <= newRange.deciMax.value)
    }
  }

  test("HourAngleRange.apply") {
    forAll { range: HourAngleRange =>
      assert(range.deciMin.value <= range.deciMax.value)
    }
  }

  test("HourAngleRange.setMin") {
    forAll { (min: HourAngleRange.IntDeciHour, range: HourAngleRange) =>
      val newRange = range.setMin(min)
      assertEquals(min, newRange.deciMin)
      assert(newRange.deciMin.value <= newRange.deciMax.value)
    }
  }

  test("HourAngleRange.setMax") {
    forAll { (max: HourAngleRange.IntDeciHour, range: HourAngleRange) =>
      val newRange = range.setMax(max)
      assertEquals(max, newRange.deciMax)
      assert(newRange.deciMin.value <= newRange.deciMax.value)
    }
  }
}

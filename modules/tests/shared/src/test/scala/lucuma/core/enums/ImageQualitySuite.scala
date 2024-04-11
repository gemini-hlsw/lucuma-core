// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.util.arb.ArbEnumerated
import munit.DisciplineSuite
import org.scalacheck.Prop.*

import java.math.RoundingMode

final class ImageQualitySuite extends DisciplineSuite {

  import ArbEnumerated.given

  test("toArcSeconds") {
    forAll { (a: ImageQuality) =>
      assertEquals(
        a.toArcSeconds.value.toBigDecimal(6, RoundingMode.HALF_UP),
        Angle.signedDecimalArcseconds.get(a.toAngle)
      )
    }
  }

  test("toAngle") {
    forAll { (a: ImageQuality) =>
      assertEquals(
        a.toAngle.toMicroarcseconds,
        a.toDeciArcSeconds.value.value * 100_000L
      )
    }
  }

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.scalacheck.numeric.given
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbQuantity
import lucuma.core.util.arb.ArbNewType
import munit.DisciplineSuite
import org.scalacheck.Prop.*

import java.math.MathContext
import java.math.RoundingMode

final class ImageQualitySuite extends DisciplineSuite {
  import ArbNewType.given
  import ArbQuantity.given

  test("toArcSeconds") {
    forAll { (a: ImageQuality) =>
      assertEquals(
        a.toArcSeconds.round(MathContext(6, RoundingMode.HALF_UP)),
        Angle.signedDecimalArcseconds.get(a.toAngle)
      )
    }
  }

  test("toAngle") {
    forAll { (a: ImageQuality) =>
      assertEquals(
        a.toAngle.toMicroarcseconds,
        a.toCentiArcSeconds * 10_000L
      )
    }
  }

}

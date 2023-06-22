// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units.{*, given}
import lucuma.core.util.arb.ArbEnumerated
import munit.DisciplineSuite
import org.scalacheck.Prop.*

final class ImageQualitySuite extends DisciplineSuite {

  import ArbEnumerated.*

  test("toAngle") {
    forAll { (a: ImageQuality) =>
      assertEquals(
        a.toAngle.toMicroarcseconds,
        a.toDeciArcSeconds.tToUnit[MicroArcSecond].value.value.toLong
      )
    }
  }

}

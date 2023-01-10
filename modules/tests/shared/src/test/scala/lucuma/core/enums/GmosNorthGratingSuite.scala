// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import munit.DisciplineSuite
import spire.math.Rational

final class GmosNorthGratingSuite extends DisciplineSuite {

  test("dispersion") {
    assertEquals(GmosNorthGrating.B1200_G5301.dispersion.value, Rational(26, 1000))
  }

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.`enum`

import munit.DisciplineSuite
import spire.math.Rational

final class GmosNorthDisperserSuite extends DisciplineSuite {

  test("dispersion") {
    assertEquals(GmosNorthDisperser.B1200_G5301.dispersion.value, Rational(26, 1000))
  }

}

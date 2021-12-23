// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import munit.DisciplineSuite
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import shapeless.tag

final class BandSuite extends DisciplineSuite {

  test("default units") {
    assertEquals(
      Band.SloanR.defaultUnit[Surface],
      tag[Brightness[Surface]](UnitOfMeasure[ABMagnitudePerArcsec2])
    )

    assertEquals(
      Band.R.defaultUnit[Integrated],
      tag[Brightness[Integrated]](UnitOfMeasure[VegaMagnitude])
    )
  }
}

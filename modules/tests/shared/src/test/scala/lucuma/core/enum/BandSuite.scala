// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.laws.EnumeratedTests
import munit.DisciplineSuite
import shapeless.tag

final class BandSuite extends DisciplineSuite {
  import ArbEnumerated._

  test("default units") {
    assertEquals(
      Band.SloanR.defaultUnits[Surface],
      tag[Brightness[Surface]](UnitOfMeasure[ABMagnitudePerArcsec2])
    )

    assertEquals(
      Band.R.defaultUnits[Integrated],
      tag[Brightness[Integrated]](UnitOfMeasure[VegaMagnitude])
    )
  }

  // Typeclasses
  checkAll(s"Enumerated[Band]", EnumeratedTests[Band].enumerated)
}

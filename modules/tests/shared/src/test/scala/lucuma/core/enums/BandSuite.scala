// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.util.*
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.laws.EnumeratedTests
import munit.DisciplineSuite

class BandSuite extends DisciplineSuite {
  import ArbEnumerated.given

  test("default units") {
    assertEquals(
      Band.SloanR.defaultUnits[Surface],
      UnitOfMeasure[ABMagnitudePerArcsec2].tag[Brightness[Surface]]
    )

    assertEquals(
      Band.R.defaultUnits[Integrated],
      UnitOfMeasure[VegaMagnitude].tag[Brightness[Integrated]]
    )
  }

  // Typeclasses
  checkAll(s"Enumerated[Band]", EnumeratedTests[Band].enumerated)
}

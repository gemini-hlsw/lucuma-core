// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import lucuma.core.math.BrightnessUnits
import lucuma.core.math.dimensional._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.laws.EnumeratedTests
import munit.DisciplineSuite

final class BrightnessUnitsSuite extends DisciplineSuite {
  import ArbEnumerated._
  import BrightnessUnits._

  // Type classes
  checkAll(
    "Units Of Brightness[Integrated]",
    EnumeratedTests[Units Of Brightness[Integrated]].enumerated
  )
  checkAll(
    "Units Of Brightness[Surface]",
    EnumeratedTests[Units Of Brightness[Surface]].enumerated
  )
  checkAll(
    "Units Of LineFlux[Integrated]",
    EnumeratedTests[Units Of LineFlux[Integrated]].enumerated
  )
  checkAll(
    "Units Of LineFlux[Surface]",
    EnumeratedTests[Units Of LineFlux[Surface]].enumerated
  )
  checkAll(
    "Units Of [FluxDensityContinuum[Integrated]]",
    EnumeratedTests[Units Of FluxDensityContinuum[Integrated]].enumerated
  )
  checkAll(
    "Units Of [FluxDensityContinuum[Surface]]",
    EnumeratedTests[Units Of FluxDensityContinuum[Surface]].enumerated
  )

  // Tag Conversions
  test("Tag conversion Brightness[Integrated] -> Brightness[Surface]") {
    assertEquals(
      VegaMagnitudeIsIntegratedBrightnessUnit.unit.toTag[Brightness[Surface]],
      VegaMagnitudePerArcsec2IsSurfaceBrightnessUnit.unit
    )
  }

  test("Tag conversion identity") {
    assertEquals(
      ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit.unit
        .toTag[FluxDensityContinuum[Surface]],
      ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit.unit
    )

  }

  test("Tag conversion roundtrip") {
    assertEquals(
      ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit.unit
        .toTag[FluxDensityContinuum[Integrated]]
        .toTag[FluxDensityContinuum[Surface]],
      ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit.unit
    )
  }
}

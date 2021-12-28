// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import lucuma.core.math.BrightnessUnits
import lucuma.core.math.dimensional.UnitType
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.laws.EnumeratedTests
import munit.DisciplineSuite
import shapeless.tag.@@

final class BrightnessUnitsSuite extends DisciplineSuite {
  import ArbEnumerated._
  import BrightnessUnits._

  // Type classes
  checkAll(
    "UnitType @@ Brightness[Integrated]",
    EnumeratedTests[UnitType @@ Brightness[Integrated]].enumerated
  )
  checkAll(
    "UnitType @@ Brightness[Surface]",
    EnumeratedTests[UnitType @@ Brightness[Surface]].enumerated
  )
  checkAll(
    "UnitType @@ LineFlux[Integrated]",
    EnumeratedTests[UnitType @@ LineFlux[Integrated]].enumerated
  )
  checkAll(
    "UnitType @@ LineFlux[Surface]",
    EnumeratedTests[UnitType @@ LineFlux[Surface]].enumerated
  )
  checkAll(
    "UnitType @@ [FluxDensityContinuum[Integrated]]",
    EnumeratedTests[UnitType @@ FluxDensityContinuum[Integrated]].enumerated
  )
  checkAll(
    "UnitType @@ [FluxDensityContinuum[Surface]]",
    EnumeratedTests[UnitType @@ FluxDensityContinuum[Surface]].enumerated
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

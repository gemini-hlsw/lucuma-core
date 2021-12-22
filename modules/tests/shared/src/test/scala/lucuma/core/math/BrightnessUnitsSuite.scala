// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import munit.DisciplineSuite
import lucuma.core.math.BrightnessUnits
import lucuma.core.util.laws.EnumeratedTests
import lucuma.core.math.dimensional.GroupedUnitType
import lucuma.core.util.arb.ArbEnumerated

final class BrightnessUnitsSuite extends DisciplineSuite {
  import ArbEnumerated._
  import BrightnessUnits._

  checkAll(
    "GroupedUnitType[Brightness[Integrated]]",
    EnumeratedTests[GroupedUnitType[Brightness[Integrated]]].enumerated
  )
  checkAll(
    "GroupedUnitType[Brightness[Surface]]",
    EnumeratedTests[GroupedUnitType[Brightness[Surface]]].enumerated
  )
  checkAll(
    "GroupedUnitType[LineFlux[Integrated]]",
    EnumeratedTests[GroupedUnitType[Brightness[Integrated]]].enumerated
  )
  checkAll(
    "GroupedUnitType[LineFlux[Surface]]",
    EnumeratedTests[GroupedUnitType[Brightness[Integrated]]].enumerated
  )
  checkAll(
    "GroupedUnitType[FluxDensityContinuum[Integrated]]",
    EnumeratedTests[GroupedUnitType[Brightness[Integrated]]].enumerated
  )
  checkAll(
    "GroupedUnitType[FluxDensityContinuum[Surface]]",
    EnumeratedTests[GroupedUnitType[Brightness[Integrated]]].enumerated
  )

}

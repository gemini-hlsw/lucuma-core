// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.kernel.laws.discipline._
import monocle.law.discipline._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.math.dimensional.arb.ArbQty._

class GroupedUnitQtySuite extends munit.DisciplineSuite {
  // Laws
  checkAll(
    "GroupedUnitQty[BigDecimal, Brightness[Surface]]",
    EqTests[GroupedUnitQty[BigDecimal, Brightness[Surface]]].eqv
  )

  // Optics
  checkAll(
    "GroupedUnitQty[BigDecimal, Brightness[Surface]].value",
    LensTests(GroupedUnitQty.value[BigDecimal, Brightness[Surface]])
  )
  checkAll(
    "GroupedUnitQty[BigDecimal, Brightness[Surface]].unit",
    LensTests(GroupedUnitQty.unit[BigDecimal, Brightness[Surface]])
  )
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.kernel.laws.discipline._
import monocle.law.discipline._
import lucuma.core.math.BrightnessUnit._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.math.dimensional.arb.ArbQty._

class GroupedUnitQuantitySuite extends munit.DisciplineSuite {
  // Laws
  checkAll(
    "GroupedUnitQuantity[BigDecimal, Brightness[Surface]]",
    EqTests[GroupedUnitQuantity[BigDecimal, Brightness[Surface]]].eqv
  )

  // Optics
  checkAll(
    "GroupedUnitQuantity[BigDecimal, Brightness[Surface]].value",
    LensTests(GroupedUnitQuantity.value[BigDecimal, Brightness[Surface]])
  )
  checkAll(
    "GroupedUnitQuantity[BigDecimal, Brightness[Surface]].unit",
    LensTests(GroupedUnitQuantity.unit[BigDecimal, Brightness[Surface]])
  )
}

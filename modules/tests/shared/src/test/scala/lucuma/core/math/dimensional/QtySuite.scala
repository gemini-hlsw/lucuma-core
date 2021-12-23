// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.kernel.laws.discipline._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.util.arb.ArbEnumerated._
import monocle.law.discipline._
import shapeless.tag.@@

class QtySuite extends munit.DisciplineSuite {
  import ArbQty._

  // Laws
  checkAll("Qty[BigDecimal]", EqTests[Qty[BigDecimal]].eqv)
  checkAll(
    "Qty[BigDecimal] @@ Brightness[Integrated]",
    EqTests[Qty[BigDecimal] @@ Brightness[Integrated]].eqv
  )

  // Optics
  checkAll("Qty[BigDecimal].value", LensTests(Qty.value[BigDecimal]))
  checkAll("Qty[BigDecimal].valueT", LensTests(Qty.valueT[BigDecimal, Brightness[Surface]]))
  checkAll("Qty[BigDecimal].unit", LensTests(Qty.unit[BigDecimal]))
  checkAll("Qty[BigDecimal].unitT", LensTests(Qty.unitT[BigDecimal, Brightness[Surface]]))
}

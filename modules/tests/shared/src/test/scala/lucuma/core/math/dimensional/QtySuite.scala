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
  checkAll("Qty[BigDecimal].valueT", LensTests(Qty.valueTagged[BigDecimal, Brightness[Surface]]))
  checkAll("Qty[BigDecimal].unit", LensTests(Qty.unit[BigDecimal]))
  checkAll("Qty[BigDecimal].unitT", LensTests(Qty.unitTagged[BigDecimal, Brightness[Surface]]))

  // Tag Conversions
  test("Tag conversion Brightness[Surface] -> Brightness[Integrated]") {
    assertEquals(
      WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit
        .withValueTagged(1)
        .toTag[LineFlux[Integrated]],
      WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(1)
    )
  }

  test("Tag conversion identity") {
    assertEquals(
      WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit
        .withValueTagged(1)
        .toTag[LineFlux[Surface]],
      WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(1)
    )
  }

  test("Tag conversion roundtrip") {
    assertEquals(
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit
        .withValueTagged(1)
        .toTag[FluxDensityContinuum[Surface]]
        .toTag[FluxDensityContinuum[Integrated]],
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit.withValueTagged(1)
    )
  }
}

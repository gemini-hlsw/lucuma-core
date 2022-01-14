// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import cats.kernel.laws.discipline._
import cats.syntax.all._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.dimensional.arb.ArbUnits
import lucuma.core.util.arb.ArbEnumerated._
import monocle.law.discipline._
import org.scalacheck.Prop._

class MeasureSuite extends munit.DisciplineSuite {
  import ArbMeasure._
  import ArbUnits._

  // Laws
  checkAll("Measure[BigDecimal]", EqTests[Measure[BigDecimal]].eqv)
  checkAll(
    "Measure[BigDecimal] Of Brightness[Integrated]",
    EqTests[Measure[BigDecimal] Of Brightness[Integrated]].eqv
  )

  test("Equality must be natural") {
    forAll { (a: Measure[BigDecimal], b: Measure[BigDecimal]) =>
      assertEquals(a.equals(b), Eq[Measure[BigDecimal]].eqv(a, b))
    }
  }

  test("Derived Display") {
    import lucuma.core.math.BrightnessValue
    import lucuma.core.math.units._
    import lucuma.core.syntax.display._

    val m = UnitOfMeasure[ABMagnitude].withValue(BrightnessValue.fromDouble(1.235))

    assertEquals(m.shortName, "1.235 AB mag")
    assertEquals(m.withError(BrightnessValue.fromDouble(0.005)).shortName, "1.235 ± 0.005 AB mag")
  }

  // Optics
  checkAll("Measure[BigDecimal].value", LensTests(Measure.value[BigDecimal]))
  checkAll(
    "Measure[BigDecimal].valueTagged",
    LensTests(Measure.valueTagged[BigDecimal, Brightness[Surface]])
  )
  checkAll("Measure[BigDecimal].units", LensTests(Measure.units[BigDecimal]))
  checkAll(
    "Measure[BigDecimal].unitsTagged",
    LensTests(Measure.unitsTagged[BigDecimal, Brightness[Surface]])
  )
  checkAll("Measure[BigDecimal].error", LensTests(Measure.error[BigDecimal]))
  checkAll(
    "Measure[BigDecimal].errorTagged",
    LensTests(Measure.errorTagged[BigDecimal, Brightness[Surface]])
  )

  // Tag Conversions
  test("Tag conversion Brightness[Surface] -> Brightness[Integrated]") {
    assertEquals(
      WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit
        .withValueTagged(5, 1.some)
        .toTag[LineFlux[Integrated]],
      WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(5, 1.some)
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
        .withValueTagged(5, 1.some)
        .toTag[FluxDensityContinuum[Surface]]
        .toTag[FluxDensityContinuum[Integrated]],
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit.withValueTagged(5, 1.some)
    )
  }
}

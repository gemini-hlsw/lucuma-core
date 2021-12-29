// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.implicits._
import cats.kernel.laws.discipline._
import coulomb.cats.implicits._
import coulomb.scalacheck.ArbQuantity
import eu.timepit.refined.cats._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.model.arb.ArbEmissionLine
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import munit._
import lucuma.core.math.Wavelength
import coulomb._
import lucuma.core.math.units._

final class EmissionLineSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbEmissionLine._
  import ArbWavelength._
  import ArbRefined._
  import ArbMeasure._
  import ArbQuantity._

  // Brightness type conversions
  val e1Integrated: EmissionLine[Integrated] =
    EmissionLine(
      Wavelength.Min,
      PosBigDecimalOne.withUnit[KilometersPerSecond],
      WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(PosBigDecimalOne)
    )
  val e1Surface: EmissionLine[Surface]       =
    EmissionLine(
      Wavelength.Min,
      PosBigDecimalOne.withUnit[KilometersPerSecond],
      WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(PosBigDecimalOne)
    )
  test("Brightness type conversion Integrated -> Surface") {
    assertEquals(e1Integrated.to[Surface], e1Surface)
  }

  test("Brightness identity type conversion") {
    assertEquals(e1Integrated.to[Integrated], e1Integrated)
  }

  test("Brightness type conversion roundtrip") {
    assertEquals(e1Surface.to[Integrated].to[Surface], e1Surface)
  }

  // Type classes
  checkAll("Eq[EmissionLine[Integrated]]", EqTests[EmissionLine[Integrated]].eqv)
  checkAll("Eq[EmissionLine[Surface]]", EqTests[EmissionLine[Surface]].eqv)

  // Optics
  checkAll("EmissionLine.wavelength[Integrated]", LensTests(EmissionLine.wavelength[Integrated]))
  checkAll("EmissionLine.lineWidth[Integrated]", LensTests(EmissionLine.lineWidth[Integrated]))
  checkAll("EmissionLine.lineFlux[Integrated]", LensTests(EmissionLine.lineFlux[Integrated]))
}

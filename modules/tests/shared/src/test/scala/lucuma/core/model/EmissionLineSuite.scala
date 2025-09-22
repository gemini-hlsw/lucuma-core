// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.implicits.*
import cats.kernel.laws.discipline.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units.*
import lucuma.core.model.arb.ArbEmissionLine
import lucuma.core.refined.given
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbNewType
import monocle.law.discipline.LensTests
import munit.*

final class EmissionLineSuite extends DisciplineSuite {
  import ArbEnumerated.given
  import ArbEmissionLine.given
  import ArbMeasure.given
  import ArbNewType.given
  import ArbRefined.given

  // Brightness type conversions
  val e1Integrated: EmissionLine[Integrated] =
    EmissionLine(
      LineWidthValue.unsafeFrom(1).withUnit[KilometersPerSecond],
      WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(LineFluxValue.unsafeFrom(1))
    )
  val e1Surface: EmissionLine[Surface]       =
    EmissionLine(
      LineWidthValue.unsafeFrom(1).withUnit[KilometersPerSecond],
      WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(LineFluxValue.unsafeFrom(1))
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

  // Typeclasses
  checkAll("Eq[EmissionLine[Integrated]]", EqTests[EmissionLine[Integrated]].eqv)
  checkAll("Eq[EmissionLine[Surface]]", EqTests[EmissionLine[Surface]].eqv)

  // Optics
  checkAll("EmissionLine.lineFlux[Integrated]", LensTests(EmissionLine.lineFlux[Integrated]))
}

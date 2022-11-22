// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.data.NonEmptyMap
import cats.implicits.*
import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import coulomb.syntax.*
import eu.timepit.refined.cats.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.arb.*
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units.*
import lucuma.core.model.arb.ArbEmissionLine
import lucuma.core.model.arb.ArbSpectralDefinition
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import munit.*
import org.typelevel.cats.time.instantInstances

import java.time.Instant

final class EmissionLineSuite extends DisciplineSuite {
  import ArbEnumerated.*
  import ArbEmissionLine.given
  import ArbRefined.*
  import ArbMeasure.given
  import ArbSpectralDefinition.*
  import ArbQuantity.given

  // Brightness type conversions
  val e1Integrated: EmissionLine[Integrated] =
    EmissionLine(
      PosBigDecimalOne.withUnit[KilometersPerSecond],
      NonEmptyMap.of(Instant.MIN -> WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(PosBigDecimalOne))
    )
  val e1Surface: EmissionLine[Surface]       =
    EmissionLine(
      PosBigDecimalOne.withUnit[KilometersPerSecond],
      NonEmptyMap.of(Instant.MIN -> WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(PosBigDecimalOne))
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
  checkAll("EmissionLine.lineWidth[Integrated]", LensTests(EmissionLine.lineWidth[Integrated]))
  checkAll("EmissionLine.lineFlux[Integrated]", LensTests(EmissionLine.lineFlux[Integrated]))
}

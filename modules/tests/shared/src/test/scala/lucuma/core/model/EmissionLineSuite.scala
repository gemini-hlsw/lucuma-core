// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.data.NonEmptyMap
import cats.implicits._
import cats.kernel.laws.discipline._
import cats.laws.discipline.arbitrary.*
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import coulomb.syntax.*
import eu.timepit.refined.cats._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.arb._
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units._
import lucuma.core.model.arb.ArbEmissionLine
import lucuma.core.model.arb.ArbSpectralDefinition
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimestamp
import monocle.law.discipline.LensTests
import munit._

final class EmissionLineSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbTimestamp._
  import ArbEmissionLine._
  import ArbRefined._
  import ArbMeasure._
  import ArbSpectralDefinition.*
  import ArbQuantity.given

  // Brightness type conversions
  val e1Integrated: EmissionLine[Integrated] =
    EmissionLine(
      PosBigDecimalOne.withUnit[KilometersPerSecond],
      NonEmptyMap.of(Timestamp.Min -> WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(PosBigDecimalOne))
    )
  val e1Surface: EmissionLine[Surface]       =
    EmissionLine(
      PosBigDecimalOne.withUnit[KilometersPerSecond],
      NonEmptyMap.of(Timestamp.Min -> WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(PosBigDecimalOne))
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

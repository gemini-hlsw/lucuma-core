// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import munit._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.math.BrightnessUnits._
import lucuma.core.model.arb.ArbEmissionLine
import monocle.law.discipline.LensTests
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.arb.ArbQty
import coulomb.scalacheck.ArbQuantity
import coulomb.cats.implicits._
import cats.implicits._

final class EmissionLineSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbEmissionLine._
  import ArbWavelength._
  import ArbRefined._
  import ArbQty._
  import ArbQuantity._

  checkAll("Eq[EmissionLine[Integrated]]", EqTests[EmissionLine[Integrated]].eqv)
  checkAll("Eq[EmissionLine[Surface]]", EqTests[EmissionLine[Surface]].eqv)

  checkAll("EmissionLine.wavelength[Integrated]", LensTests(EmissionLine.wavelength[Integrated]))
  checkAll("EmissionLine.lineWidth[Integrated]", LensTests(EmissionLine.lineWidth[Integrated]))
  checkAll("EmissionLine.lineFlux[Integrated]", LensTests(EmissionLine.lineFlux[Integrated]))
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import coulomb.scalacheck.ArbQuantity
import eu.timepit.refined.cats._
import lucuma.core.math.dimensional.arb.ArbQty
import monocle.law.discipline.PrismTests
import coulomb.cats.implicits._

final class SpectralDistributionSuite extends DisciplineSuite {
  import SpectralDistribution._
  import ArbSpectralDistribution._
  import BrightnessUnits._
  import ArbEnumerated._
  import ArbQuantity._
  import ArbQty._
  import ArbRefined._

  // Laws
  checkAll("Order[CoolStarModel]", OrderTests[CoolStarModel].order)
  checkAll("Order[PowerLaw]", OrderTests[PowerLaw].order)
  checkAll("Order[BlackBody]", OrderTests[BlackBody].order)

  checkAll(
    "Eq[SpectralDistribution[Integrated]]",
    EqTests[SpectralDistribution[Integrated]].eqv
  )
  checkAll(
    "Eq[SpectralDistribution[Surface]]",
    EqTests[SpectralDistribution[Surface]].eqv
  )

  // Optics
  checkAll("StellarLibrary.librarySpectrum", LensTests(StellarLibrary.librarySpectrum))
  checkAll("CoolStarModel.temperature", LensTests(CoolStarModel.temperature))
  checkAll("Galaxy.galaxySpectrum", LensTests(Galaxy.galaxySpectrum))
  checkAll("Planet.planetSpectrum", LensTests(Planet.planetSpectrum))
  checkAll("Quasar.quasarSpectrum", LensTests(Quasar.quasarSpectrum))
  checkAll("HIIRegion.hiiRegionSpectrum", LensTests(HIIRegion.hiiRegionSpectrum))
  checkAll(
    "PlanetaryNebula.planetaryNebulaSpectrum",
    LensTests(PlanetaryNebula.planetaryNebulaSpectrum)
  )
  checkAll("EmissionLines[Integrated].lines", LensTests(EmissionLines.lines[Integrated]))
  checkAll("EmissionLines[Surface].lines", LensTests(EmissionLines.lines[Surface]))
  checkAll(
    "EmissionLines[Integrated].fluxDensityContinuum",
    LensTests(EmissionLines.fluxDensityContinuum[Integrated])
  )
  checkAll(
    "EmissionLines[Surface].fluxDensityContinuum",
    LensTests(EmissionLines.fluxDensityContinuum[Surface])
  )
  checkAll("PowerLaw.index", LensTests(PowerLaw.index))
  checkAll("BlackBody.temperature", LensTests(BlackBody.temperature))

  checkAll(
    "SpectralDistribution[Integrated].stellarLibrary",
    PrismTests(SpectralDistribution.stellarLibrary[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].stellarLibrary",
    PrismTests(SpectralDistribution.stellarLibrary[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].coolStarModel",
    PrismTests(SpectralDistribution.coolStarModel[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].coolStarModel",
    PrismTests(SpectralDistribution.coolStarModel[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].galaxy",
    PrismTests(SpectralDistribution.galaxy[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].galaxy",
    PrismTests(SpectralDistribution.galaxy[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].planet",
    PrismTests(SpectralDistribution.planet[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].planet",
    PrismTests(SpectralDistribution.planet[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].quasar",
    PrismTests(SpectralDistribution.quasar[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].quasar",
    PrismTests(SpectralDistribution.quasar[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].hiiRegion",
    PrismTests(SpectralDistribution.hiiRegion[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].hiiRegion",
    PrismTests(SpectralDistribution.hiiRegion[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].planetaryNebula",
    PrismTests(SpectralDistribution.planetaryNebula[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].planetaryNebula",
    PrismTests(SpectralDistribution.planetaryNebula[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].emissionLines",
    PrismTests(SpectralDistribution.emissionLines[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].emissionLines",
    PrismTests(SpectralDistribution.emissionLines[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].powerLaw",
    PrismTests(SpectralDistribution.powerLaw[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].powerLaw",
    PrismTests(SpectralDistribution.powerLaw[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].blackBody",
    PrismTests(SpectralDistribution.blackBody[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].blackBody",
    PrismTests(SpectralDistribution.blackBody[Surface])
  )
  checkAll(
    "SpectralDistribution[Integrated].userDefined",
    PrismTests(SpectralDistribution.userDefined[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].userDefined",
    PrismTests(SpectralDistribution.userDefined[Surface])
  )
}

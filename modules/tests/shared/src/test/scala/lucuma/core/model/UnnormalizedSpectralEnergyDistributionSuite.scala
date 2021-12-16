// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import coulomb.scalacheck.ArbQuantity
import eu.timepit.refined.cats._
import monocle.law.discipline.PrismTests
import coulomb.cats.implicits._

final class UnnormalizedSpectralEnergyDistributionSuite extends DisciplineSuite {
  import UnnormalizedSpectralEnergyDistribution._
  import ArbUnnormalizedSpectralEnergyDistribution._
  import ArbEnumerated._
  import ArbQuantity._
  import ArbRefined._

  // Laws
  checkAll("Order[CoolStarModel]", OrderTests[CoolStarModel].order)
  checkAll("Order[PowerLaw]", OrderTests[PowerLaw].order)
  checkAll("Order[BlackBody]", OrderTests[BlackBody].order)

  checkAll(
    "Eq[UnnormalizedSpectralEnergyDistribution]",
    EqTests[UnnormalizedSpectralEnergyDistribution].eqv
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
  checkAll("PowerLaw.index", LensTests(PowerLaw.index))
  checkAll("BlackBody.temperature", LensTests(BlackBody.temperature))

  checkAll(
    "UnnormalizedSpectralEnergyDistribution.stellarLibrary",
    PrismTests(UnnormalizedSpectralEnergyDistribution.stellarLibrary)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.coolStarModel",
    PrismTests(UnnormalizedSpectralEnergyDistribution.coolStarModel)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.galaxy",
    PrismTests(UnnormalizedSpectralEnergyDistribution.galaxy)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.planet",
    PrismTests(UnnormalizedSpectralEnergyDistribution.planet)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.quasar",
    PrismTests(UnnormalizedSpectralEnergyDistribution.quasar)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.hiiRegion",
    PrismTests(UnnormalizedSpectralEnergyDistribution.hiiRegion)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.planetaryNebula",
    PrismTests(UnnormalizedSpectralEnergyDistribution.planetaryNebula)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.powerLaw",
    PrismTests(UnnormalizedSpectralEnergyDistribution.powerLaw)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.blackBody",
    PrismTests(UnnormalizedSpectralEnergyDistribution.blackBody)
  )
  checkAll(
    "UnnormalizedSpectralEnergyDistribution.userDefined",
    PrismTests(UnnormalizedSpectralEnergyDistribution.userDefined)
  )
}

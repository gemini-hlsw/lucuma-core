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
  import USED._
  import ArbUSED._
  import ArbEnumerated._
  import ArbQuantity._
  import ArbRefined._

  // Laws
  checkAll("Order[CoolStarModel]", OrderTests[CoolStarModel].order)
  checkAll("Order[PowerLaw]", OrderTests[PowerLaw].order)
  checkAll("Order[BlackBody]", OrderTests[BlackBody].order)

  checkAll(
    "Eq[USED]",
    EqTests[USED].eqv
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
    "USED.stellarLibrary",
    PrismTests(USED.stellarLibrary)
  )
  checkAll(
    "USED.coolStarModel",
    PrismTests(USED.coolStarModel)
  )
  checkAll(
    "USED.galaxy",
    PrismTests(USED.galaxy)
  )
  checkAll(
    "USED.planet",
    PrismTests(USED.planet)
  )
  checkAll(
    "USED.quasar",
    PrismTests(USED.quasar)
  )
  checkAll(
    "USED.hiiRegion",
    PrismTests(USED.hiiRegion)
  )
  checkAll(
    "USED.planetaryNebula",
    PrismTests(USED.planetaryNebula)
  )
  checkAll(
    "USED.powerLaw",
    PrismTests(USED.powerLaw)
  )
  checkAll(
    "USED.blackBody",
    PrismTests(USED.blackBody)
  )
  checkAll(
    "USED.userDefined",
    PrismTests(USED.userDefined)
  )
}

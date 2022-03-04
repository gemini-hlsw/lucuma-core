// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import coulomb.cats.implicits._
import coulomb.scalacheck.ArbQuantity
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import monocle.law.discipline.PrismTests
import munit._

final class UnnormalizedSEDSuite extends DisciplineSuite {
  import UnnormalizedSED._
  import ArbUnnormalizedSED._
  import ArbEnumerated._
  import ArbQuantity._
  import ArbRefined._

  // Laws
  checkAll("Order[CoolStarModel]", OrderTests[CoolStarModel].order)
  checkAll("Order[PowerLaw]", OrderTests[PowerLaw].order)
  checkAll("Order[BlackBody]", OrderTests[BlackBody].order)

  checkAll(
    "Eq[UnnormalizedSED]",
    EqTests[UnnormalizedSED].eqv
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
    "UnnormalizedSED.stellarLibrary",
    PrismTests(UnnormalizedSED.stellarLibrary)
  )
  checkAll(
    "UnnormalizedSED.coolStarModel",
    PrismTests(UnnormalizedSED.coolStarModel)
  )
  checkAll(
    "UnnormalizedSED.galaxy",
    PrismTests(UnnormalizedSED.galaxy)
  )
  checkAll(
    "UnnormalizedSED.planet",
    PrismTests(UnnormalizedSED.planet)
  )
  checkAll(
    "UnnormalizedSED.quasar",
    PrismTests(UnnormalizedSED.quasar)
  )
  checkAll(
    "UnnormalizedSED.hiiRegion",
    PrismTests(UnnormalizedSED.hiiRegion)
  )
  checkAll(
    "UnnormalizedSED.planetaryNebula",
    PrismTests(UnnormalizedSED.planetaryNebula)
  )
  checkAll(
    "UnnormalizedSED.powerLaw",
    PrismTests(UnnormalizedSED.powerLaw)
  )
  checkAll(
    "UnnormalizedSED.blackBody",
    PrismTests(UnnormalizedSED.blackBody)
  )
  checkAll(
    "UnnormalizedSED.userDefined",
    PrismTests(UnnormalizedSED.userDefined)
  )
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import coulomb.ops.algebra.cats.all.given
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.*
import lucuma.core.model.arb.*
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import monocle.law.discipline.PrismTests
import munit.*

final class UnnormalizedSEDSuite extends DisciplineSuite {
  import UnnormalizedSED.*
  import ArbUnnormalizedSED.given
  import ArbEnumerated.given
  import ArbQuantity.given
  import ArbRefined.given

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
  checkAll(
    "UnnormalizedSED.userDefinedAttachment",
    PrismTests(UnnormalizedSED.userDefinedAttachment)
  )
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._
import lucuma.core.math.BrightnessUnit
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import monocle.law.discipline.OptionalTests
import coulomb.scalacheck.ArbQuantity
import eu.timepit.refined.cats._
import lucuma.core.math.dimensional.arb.ArbQty
import monocle.law.discipline.PrismTests
import coulomb.cats.implicits._

final class SpectralDistributionSuite extends DisciplineSuite {
  import SpectralDistribution._
  import ArbSpectralDistribution._
  import BrightnessUnit._
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
  checkAll("Library.spectrum", LensTests(Library.spectrum))
  checkAll("Library.stellar", OptionalTests(Library.stellar))
  checkAll("Library.nonstellar", OptionalTests(Library.nonstellar))
  checkAll("CoolStarModel.temperature", LensTests(CoolStarModel.temperature))
  checkAll("EmissionLine[Integrated].line", LensTests(EmissionLine.line[Integrated]))
  checkAll("EmissionLine[Surface].line", LensTests(EmissionLine.line[Surface]))
  checkAll("EmissionLine[Integrated].continuum", LensTests(EmissionLine.continuum[Integrated]))
  checkAll("EmissionLine[Surface].continuum", LensTests(EmissionLine.continuum[Surface]))
  checkAll("PowerLaw.index", LensTests(PowerLaw.index))
  checkAll("BlackBody.temperature", LensTests(BlackBody.temperature))

  checkAll(
    "SpectralDistribution[Integrated].library",
    PrismTests(SpectralDistribution.library[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].library",
    PrismTests(SpectralDistribution.library[Surface])
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
    "SpectralDistribution[Integrated].emissionLine",
    PrismTests(SpectralDistribution.emissionLine[Integrated])
  )
  checkAll(
    "SpectralDistribution[Surface].emissionLine",
    PrismTests(SpectralDistribution.emissionLine[Surface])
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
}

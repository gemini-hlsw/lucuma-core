// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._
import lucuma.core.math.BrightnessUnit

final class SpectralDistributionSuite extends DisciplineSuite {
  import SpectralDistribution._
  import ArbSpectralDistribution._

  // Laws
  checkAll("Order[CoolStarModel]", OrderTests[CoolStarModel].order)
  checkAll(
    "Order[EmissionLine[Integrated]]",
    OrderTests[EmissionLine[BrightnessUnit.Integrated]].order
  )
  checkAll(
    "Order[EmissionLine[Surface]]",
    OrderTests[EmissionLine[BrightnessUnit.Surface]].order
  )

  checkAll("Order[PowerLaw]", OrderTests[PowerLaw].order)
  checkAll("Order[BlackBody]", OrderTests[BlackBody].order)
  checkAll(
    "Eq[SpectralDistribution[Integrated]]",
    EqTests[SpectralDistribution[BrightnessUnit.Integrated]].eqv
  )
  checkAll(
    "Eq[SpectralDistribution[Surface]]",
    EqTests[SpectralDistribution[BrightnessUnit.Surface]].eqv
  )
}

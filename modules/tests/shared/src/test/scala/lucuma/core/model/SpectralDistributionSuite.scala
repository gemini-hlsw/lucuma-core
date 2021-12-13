// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._
import lucuma.core.math.BrightnessUnit
import lucuma.core.util.arb.ArbEnumerated

final class SpectralDistributionSuite extends DisciplineSuite {
  import SpectralDistribution._
  import ArbSpectralDistribution._
  import BrightnessUnit._
  import ArbEnumerated._

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
}

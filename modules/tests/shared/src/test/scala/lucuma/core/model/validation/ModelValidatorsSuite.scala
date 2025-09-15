// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.validation

import coulomb.*
import coulomb.integrations.cats.all.given
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.math.arb.ArbQuantity.given
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import lucuma.core.refined.given
import lucuma.core.util.arb.ArbNewType.given
import munit.DisciplineSuite

final class ModelValidatorsSuite extends DisciplineSuite {
  checkAll(
    "AirMassElevationRangeValidWedge",
    ValidWedgeTests(ModelValidators.AirMassElevationRangeValidWedge).validWedgeLaws
  )

  checkAll(
    "HourAngleElevationRangeValidWedge",
    ValidWedgeTests(ModelValidators.HourAngleElevationRangeValidWedge).validWedgeLaws
  )

  checkAll(
    "ImageQualityValidWedge",
    ValidWedgeTests(ModelValidators.ImageQualityValidWedge).validWedgeLaws
  )

  checkAll(
    "CloudExtinctionValidWedge",
    ValidWedgeTests(ModelValidators.CloudExtinctionValidWedge).validWedgeLaws
  )
}

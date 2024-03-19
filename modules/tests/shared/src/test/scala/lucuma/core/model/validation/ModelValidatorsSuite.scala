// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.validation

import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import munit.DisciplineSuite

final class ModelValidatorsSuite extends DisciplineSuite {
  checkAll(
    "airMassElevationRangeValidWedge",
    ValidWedgeTests(ModelValidators.airMassElevationRangeValidWedge).validWedgeLaws
  )

  checkAll(
    "hourAngleElevationRangeValidWedge",
    ValidWedgeTests(ModelValidators.hourAngleElevationRangeValidWedge).validWedgeLaws
  )
}

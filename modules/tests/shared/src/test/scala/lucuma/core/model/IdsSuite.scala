// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import munit.DisciplineSuite
import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests

final class IdsSuite extends DisciplineSuite {
  import ArbGid._

  checkAll("Asterism.Id", GidTests[Asterism.Id].gid)
  checkAll("Configuration.Id", GidTests[Configuration.Id].gid)
  checkAll("ConstraintSet.Id", GidTests[ConstraintSet.Id].gid)
  checkAll("Observation.Id", GidTests[Observation.Id].gid)
  checkAll("Program.Id", GidTests[Program.Id].gid)
}

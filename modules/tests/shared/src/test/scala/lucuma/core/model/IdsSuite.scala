// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests
import lucuma.core.util.laws.UidTests
import munit.DisciplineSuite

final class IdsSuite extends DisciplineSuite {
  import ArbGid._
  import ArbUid._

  checkAll("Configuration.Id", GidTests[Configuration.Id].gid)
  checkAll("Dataset.Id", GidTests[Dataset.Id].gid)
  checkAll("ExecutionEvent.Id", GidTests[ExecutionEvent.Id].gid)
  checkAll("Observation.Id", GidTests[Observation.Id].gid)
  checkAll("Program.Id", GidTests[Program.Id].gid)
  checkAll("Visit.Id", GidTests[Visit.Id].gid)
}

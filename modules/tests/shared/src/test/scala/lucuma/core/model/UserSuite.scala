// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.arb._
import munit._
import lucuma.core.util.laws.GidTests

final class UserSuite extends DisciplineSuite {
  import ArbGid._

  // Laws
  checkAll("User.Id", GidTests[User.Id].gid)

}

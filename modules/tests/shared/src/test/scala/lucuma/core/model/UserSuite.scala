// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.laws.GidTests
import munit.*

final class UserSuite extends DisciplineSuite {
  import ArbGid.given
  import ArbUser.given

  // Laws
  checkAll("User.Id", GidTests[User.Id].gid)
  checkAll("Eq[User]", EqTests[User].eqv)

}

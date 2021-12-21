// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests
import munit._

final class UserSuite extends DisciplineSuite {
  import ArbGid._
  import ArbUser._

  // Laws
  checkAll("User.Id", GidTests[User.Id].gid)
  checkAll("Eq[User]", EqTests[User].eqv)

}

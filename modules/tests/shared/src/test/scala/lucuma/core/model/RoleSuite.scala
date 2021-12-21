// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.GidTests
import munit._

final class RoleSuite extends DisciplineSuite {
  import ArbGid._
  import ArbRole._

  // Laws
  checkAll("StandardRole.Id", GidTests[StandardRole.Id].gid)
  checkAll("Eq[Role]", EqTests[Role].eqv)
  checkAll("Eq[StandardRole]", EqTests[StandardRole].eqv)

}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.model.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.laws.GidTests
import munit.*

final class RoleSuite extends DisciplineSuite {
  import ArbGid.given
  import ArbRole.given

  // Laws
  checkAll("StandardRole.Id", GidTests[StandardRole.Id].gid)
  checkAll("Eq[Role]", EqTests[Role].eqv)
  checkAll("Eq[StandardRole]", EqTests[StandardRole].eqv)

}

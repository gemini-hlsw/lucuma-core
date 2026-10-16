// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.group

import cats.kernel.laws.discipline.*
import lucuma.core.model.group.arb.ArbGroupProgress
import munit.DisciplineSuite

final class GroupProgressSuite extends DisciplineSuite:

  import ArbGroupProgress.given

  checkAll("Eq[GroupProgress]", EqTests[GroupProgress].eqv)

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.truncated

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.truncated.arb.ArbTruncatedAngle._
import munit.DisciplineSuite

class TruncatedAngleSpec extends DisciplineSuite {
  checkAll("Eq[TruncatedAngle]", EqTests[TruncatedAngle].eqv)
}

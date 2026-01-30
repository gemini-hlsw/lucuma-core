// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig.given
import munit.*

class Igrins2DynamicConfigSuite extends DisciplineSuite:
  checkAll("Eq[Igrins2DynamicConfig]", EqTests[Igrins2DynamicConfig].eqv)

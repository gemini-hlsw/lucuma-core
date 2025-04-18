// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.f2.arb.ArbF2StaticConfig.given
import munit.*

class F2StaticConfigSuite extends DisciplineSuite:

  checkAll("Eq[F2StaticConfig]", EqTests[F2StaticConfig].eqv)

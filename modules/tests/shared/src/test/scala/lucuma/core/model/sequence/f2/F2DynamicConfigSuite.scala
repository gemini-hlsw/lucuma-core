// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.f2.arb.ArbF2DynamicConfig.given
import lucuma.core.model.sequence.f2.arb.ArbF2FpuMask.given
import munit.*

class F2DynamicConfigSuite extends DisciplineSuite:
  checkAll("Eq[F2FpuMask]", EqTests[F2FpuMask].eqv)
  checkAll("Eq[F2DynamicConfig]", EqTests[F2DynamicConfig].eqv)

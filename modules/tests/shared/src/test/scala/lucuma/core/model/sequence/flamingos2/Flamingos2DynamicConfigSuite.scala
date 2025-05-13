// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig.given
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2FpuMask.given
import munit.*

class Flamingos2DynamicConfigSuite extends DisciplineSuite:
  checkAll("Eq[Flamingos2FpuMask]", EqTests[Flamingos2FpuMask].eqv)
  checkAll("Eq[Flamingos2DynamicConfig]", EqTests[Flamingos2DynamicConfig].eqv)

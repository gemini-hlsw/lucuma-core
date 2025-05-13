// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2StaticConfig.given
import munit.*

class Flamingos2StaticConfigSuite extends DisciplineSuite:

  checkAll("Eq[Flamingos2StaticConfig]", EqTests[Flamingos2StaticConfig].eqv)

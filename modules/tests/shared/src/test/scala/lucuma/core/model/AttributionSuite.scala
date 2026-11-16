// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import lucuma.core.model.arb.ArbAttribution
import munit.DisciplineSuite

final class AttributionSuite extends DisciplineSuite:

  import ArbAttribution.given

  checkAll("Eq[Attribution]", EqTests[Attribution].eqv)

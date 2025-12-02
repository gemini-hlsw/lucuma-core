// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.sequence.arb.ArbTelescopeConfig
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.*
import munit.*

class TelescopeConfigSuite extends DisciplineSuite:
  import ArbEnumerated.given
  import ArbOffset.given
  import ArbTelescopeConfig.given

  checkAll("Order[TelescopeConfig]",    OrderTests[TelescopeConfig].order)
  checkAll("TelescopeConfig.offset",    LensTests(TelescopeConfig.offset))
  checkAll("TelescopeConfig.guiding",   LensTests(TelescopeConfig.guiding))

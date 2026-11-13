// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.data.NonEmptyList
import lucuma.core.enums.ScorpioSlitOffsetPreset
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfig
import munit.*

class ScorpioSlitOffsetSuite extends FunSuite:

  test("defaultSlitTelescopeConfigs for NodAlongSlit"):
    val cfg = defaultSlitTelescopeConfigs(ScorpioSlitOffsetPreset.NodAlongSlit)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(
      cfg.telescopeConfigs,
      NonEmptyList.of[BigDecimal](0, -15, 15, 0).map(q => TelescopeConfig(Offset(0.pArcsec, q.qArcsec), StepGuideState.Enabled))
    )

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.data.NonEmptyList
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfig
import munit.*

class GmosTelescopeConfigsSuite extends FunSuite:

  private def guidedAlongSlit(qs: BigDecimal*): NonEmptyList[TelescopeConfig] =
    NonEmptyList.fromListUnsafe(
      qs.toList.map(q => TelescopeConfig(Offset(0.pArcsec, q.qArcsec), StepGuideState.Enabled))
    )

  test("long slit default nods along the slit, guided throughout"):
    val cfg = longslit.DefaultSlitTelescopeConfigs
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(cfg.telescopeConfigs, guidedAlongSlit(0, 15, -15))

  test("MOS default stays on axis so targets keep their slitlets"):
    assertEquals(
      mos.DefaultTelescopeConfigs,
      NonEmptyList.one(TelescopeConfig(Offset.Zero, StepGuideState.Enabled))
    )

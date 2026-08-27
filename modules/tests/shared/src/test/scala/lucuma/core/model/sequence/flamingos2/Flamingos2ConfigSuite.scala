// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.data.NonEmptyList
import lucuma.core.enums.Flamingos2MosOffsetPreset
import lucuma.core.enums.Flamingos2SlitOffsetPreset
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfig
import munit.*

class Flamingos2ConfigSuite extends FunSuite:

  private def alongSlit(guiding: StepGuideState, qs: BigDecimal*): NonEmptyList[TelescopeConfig] =
    NonEmptyList.fromListUnsafe(
      qs.toList.map(q => TelescopeConfig(Offset(0.pArcsec, q.qArcsec), guiding))
    )

  test("Telluric: along-slit ±15 arcsec, all guided"):
    val cfg = defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.Telluric)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(cfg.telescopeConfigs, alongSlit(StepGuideState.Enabled, 15, -15, -15, 15))

  test("MosTelluric: along-slit 60,40,20,-20,40,60 arcsec, all guided"):
    val cfg = defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.MosTelluric)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(cfg.telescopeConfigs, alongSlit(StepGuideState.Enabled, 60, 40, 20, -20, 40, 60))

  test("NodAlongSlit: along-slit ±10 arcsec, all guided"):
    val cfg = defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.NodAlongSlit)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(cfg.telescopeConfigs, alongSlit(StepGuideState.Enabled, 10, -10, -10, 10))

  test("NodToSky: (0,0),(0,300),(0,310),(0,0), guide off on the sky offsets"):
    val cfg = defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.NodToSky)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodToSky)
    assertEquals(
      cfg.telescopeConfigs,
      NonEmptyList.of(
        TelescopeConfig(Offset(0.pArcsec,   0.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(0.pArcsec, 300.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(0.pArcsec, 310.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(0.pArcsec,   0.qArcsec), StepGuideState.Enabled)
      )
    )

  test("MOS SparseField: along-slit ±1.2 arcsec, all guided"):
    val cfg = defaultMosTelescopeConfigs(Flamingos2MosOffsetPreset.SparseField)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(cfg.telescopeConfigs, alongSlit(StepGuideState.Enabled, 1.2, -1.2, -1.2, 1.2))

  test("MOS CrowdedField: (0,0),(0,300),(0,320),(0,0), guide off on the sky offsets"):
    val cfg = defaultMosTelescopeConfigs(Flamingos2MosOffsetPreset.CrowdedField)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodToSky)
    assertEquals(
      cfg.telescopeConfigs,
      NonEmptyList.of(
        TelescopeConfig(Offset(0.pArcsec,   0.qArcsec), StepGuideState.Enabled),
        TelescopeConfig(Offset(0.pArcsec, 300.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(0.pArcsec, 320.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset(0.pArcsec,   0.qArcsec), StepGuideState.Enabled)
      )
    )

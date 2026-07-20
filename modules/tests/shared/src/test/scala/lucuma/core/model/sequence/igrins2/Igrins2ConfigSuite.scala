// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.data.NonEmptyList
import cats.kernel.laws.discipline.*
import lucuma.core.enums.Igrins2FowlerSamples
import lucuma.core.enums.Igrins2SlitOffsetPreset
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig.given
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2StaticConfig.given
import lucuma.core.syntax.timespan.*
import munit.*

class Igrins2ConfigSuite extends DisciplineSuite:
  checkAll("Eq[Igrins2DynamicConfig]", EqTests[Igrins2DynamicConfig].eqv)
  checkAll("Eq[Igrins2StaticConfig]", EqTests[Igrins2StaticConfig].eqv)

  private def alongSlit(guiding: StepGuideState, qs: BigDecimal*): NonEmptyList[TelescopeConfig] =
    NonEmptyList.fromListUnsafe(
      qs.toList.map(q => TelescopeConfig(Offset(0.pArcsec, q.qArcsec), guiding))
    )

  test("fowlerSamples from exposure time"):
    assertEquals(Igrins2DynamicConfig(1000.msTimeSpan).fowlerSamples, Igrins2FowlerSamples.One)
    assertEquals(Igrins2DynamicConfig(5.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Two)
    assertEquals(Igrins2DynamicConfig(10.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Four)
    assertEquals(Igrins2DynamicConfig(15.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Eight)
    assertEquals(Igrins2DynamicConfig(30.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Sixteen)
    assertEquals(Igrins2DynamicConfig(120.secTimeSpan).fowlerSamples, Igrins2FowlerSamples.Sixteen)

  test("defaultSlitTelescopeConfigs for NodAlongSlit"):
    val cfg = defaultSlitTelescopeConfigs(Igrins2SlitOffsetPreset.NodAlongSlit)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodAlongSlit)
    assertEquals(cfg.telescopeConfigs, alongSlit(StepGuideState.Enabled, -1.25, 1.25, 1.25, -1.25))

  test("defaultSlitTelescopeConfigs for NodToSky"):
    val cfg = defaultSlitTelescopeConfigs(Igrins2SlitOffsetPreset.NodToSky)
    assertEquals(cfg.offsetsType, SlitOffsetMode.NodToSky)
    assertEquals(
      cfg.telescopeConfigs,
      NonEmptyList.of(
        TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
        TelescopeConfig(Offset(10.pArcsec, 10.qArcsec), StepGuideState.Disabled),
        TelescopeConfig(Offset.Zero, StepGuideState.Enabled)
      )
    )

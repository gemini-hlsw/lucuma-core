// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.arb.ArbTelescopeConfig
import lucuma.core.model.sequence.arb.ArbTelescopeConfigAlongSlit
import lucuma.core.model.sequence.arb.ArbTelescopeConfigSpec
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
  checkAll("TelescopeConfig.purpose",   LensTests(TelescopeConfig.purpose))

  import ArbTelescopeConfigSpec.given
  import ArbTelescopeConfigAlongSlit.given

  checkAll("Order[TelescopeConfigSpec]",          OrderTests[TelescopeConfigSpec].order)
  checkAll("TelescopeConfigSpec.offset",          LensTests(TelescopeConfigSpec.offset))
  checkAll("TelescopeConfigSpec.guiding",         LensTests(TelescopeConfigSpec.guiding))
  checkAll("TelescopeConfigSpec.explicitPurpose", LensTests(TelescopeConfigSpec.explicitPurpose))
  checkAll("TelescopeConfigAlongSlit.explicitPurpose", LensTests(TelescopeConfigAlongSlit.explicitPurpose))

  test("an explicit purpose wins over the default"):
    val spec = TelescopeConfigSpec(Offset.Zero, StepGuideState.Enabled, OffsetPurpose.Sky.some)
    assertEquals(spec.toTelescopeConfig(_ => OffsetPurpose.Object).purpose, OffsetPurpose.Sky)

  test("an unspecified purpose takes the default for its offset"):
    val off  = Offset(0.pArcsec, 30.qArcsec)
    val spec = TelescopeConfigSpec(off, StepGuideState.Enabled)
    assertEquals(
      spec.toTelescopeConfig(o => if o === off then OffsetPurpose.Sky else OffsetPurpose.Object),
      TelescopeConfig(off, StepGuideState.Enabled, OffsetPurpose.Sky)
    )

  test("along-slit positions resolve against their (0, q) offset"):
    val pos = TelescopeConfigAlongSlit(30.qArcsec, StepGuideState.Disabled)
    assertEquals(
      pos.toTelescopeConfig(o => if o.q === 30.qArcsec then OffsetPurpose.Sky else OffsetPurpose.Object),
      TelescopeConfig(Offset(0.pArcsec, 30.qArcsec), StepGuideState.Disabled, OffsetPurpose.Sky)
    )

  test("spec / along-slit round trip keeps the explicit purpose"):
    val pos = TelescopeConfigAlongSlit(2.qArcsec, StepGuideState.Enabled, OffsetPurpose.Sky.some)
    assertEquals(pos.toTelescopeConfigSpec.toTelescopeConfigAlongSlit, pos)

  test("object positions are guided and explicitly on the object"):
    assertEquals(
      TelescopeConfigSpec.objectAt(Offset.Zero),
      TelescopeConfigSpec(Offset.Zero, StepGuideState.Enabled, OffsetPurpose.Object.some)
    )
    assertEquals(
      TelescopeConfigAlongSlit.objectAt(1.25.qArcsec),
      TelescopeConfigAlongSlit(1.25.qArcsec, StepGuideState.Enabled, OffsetPurpose.Object.some)
    )

  test("sky positions are unguided and explicitly on the sky"):
    val off = Offset(0.pArcsec, 300.qArcsec)
    assertEquals(
      TelescopeConfigSpec.skyAt(off),
      TelescopeConfigSpec(off, StepGuideState.Disabled, OffsetPurpose.Sky.some)
    )
    assertEquals(
      TelescopeConfigAlongSlit.skyAt(30.qArcsec),
      TelescopeConfigAlongSlit(30.qArcsec, StepGuideState.Disabled, OffsetPurpose.Sky.some)
    )

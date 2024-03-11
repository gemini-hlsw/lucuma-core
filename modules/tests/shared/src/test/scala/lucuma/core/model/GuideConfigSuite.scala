// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.enums.GuideProbe
import lucuma.core.model.GuideConfig.given
import lucuma.core.model.arb.ArbAltairConfig.given
import lucuma.core.model.arb.ArbGemsConfig.given
import lucuma.core.model.arb.ArbGuideConfig.given
import lucuma.core.model.arb.ArbM1GuideConfig.given
import lucuma.core.model.arb.ArbM2GuideConfig.given
import lucuma.core.model.arb.ArbProbeGuide.given
import lucuma.core.model.arb.ArbTelescopeGuideConfig.given
import lucuma.core.model.arb.*
import lucuma.core.util.arb.ArbEnumerated.given
import munit.*

class GuideConfigSuite extends DisciplineSuite {

  // Laws
  checkAll("Eq[GuideConifg]", EqTests[GuideConfig].eqv)
  checkAll("Eq[M1GuideConifg]", EqTests[M1GuideConfig].eqv)
  checkAll("Eq[M2GuideConifg]", EqTests[M2GuideConfig].eqv)
  checkAll("Eq[GemsConfig]", EqTests[GemsConfig].eqv)
  checkAll("Eq[AltairConfig]", EqTests[AltairConfig].eqv)
  checkAll("Eq[TelescopeGuideConfig]", EqTests[TelescopeGuideConfig].eqv)
  checkAll("Eq[ProbeGuide]", EqTests[ProbeGuide].eqv)
  checkAll("M1GuideConfig JSON Codec", CodecTests[M1GuideConfig].codec)
  checkAll("M2GuideConfig JSON Codec", CodecTests[M2GuideConfig].codec)
  checkAll("GemsConfig JSON Codec", CodecTests[GemsConfig].codec)
  checkAll("AltairConfig JSON Codec", CodecTests[AltairConfig].codec)
  checkAll("GuideProbe JSON Codec", CodecTests[GuideProbe].unserializableCodec)
  checkAll("ProbeGuide JSON Codec", CodecTests[ProbeGuide].unserializableCodec)
  checkAll("TelescopeConfig JSON Codec", CodecTests[TelescopeGuideConfig].unserializableCodec)

}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.enums.*
import lucuma.core.model.M1GuideConfig
import lucuma.core.model.M2GuideConfig
import lucuma.core.model.ProbeGuide
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.util.arb.ArbNewType.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

import ArbM1GuideConfig.given
import ArbM2GuideConfig.given
import ArbProbeGuide.given

trait ArbTelescopeGuideConfig {
  given Arbitrary[TelescopeGuideConfig] =
    Arbitrary {
      for {
        mo <- arbitrary[MountGuideOption]
        m1 <- arbitrary[M1GuideConfig]
        m2 <- arbitrary[M2GuideConfig]
        m  <- arbitrary[Option[Boolean]]
        pg <- arbitrary[Option[ProbeGuide]]
      } yield TelescopeGuideConfig(mo, m1, m2, m, pg)
    }

  given Cogen[TelescopeGuideConfig] =
    Cogen[(Boolean, M1GuideConfig, M2GuideConfig, Option[Boolean], Option[ProbeGuide])]
      .contramap(x => (x.mountGuide.value, x.m1Guide, x.m2Guide, x.dayTimeMode, x.probeGuide))
}

object ArbTelescopeGuideConfig extends ArbTelescopeGuideConfig

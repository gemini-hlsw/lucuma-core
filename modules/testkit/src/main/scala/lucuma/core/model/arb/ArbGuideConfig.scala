// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.AltairConfig
import lucuma.core.model.GemsConfig
import lucuma.core.model.GuideConfig
import lucuma.core.model.TelescopeGuideConfig
import lucuma.core.model.arb.ArbAltairConfig.given
import lucuma.core.model.arb.ArbGemsConfig.given
import lucuma.core.model.arb.ArbTelescopeGuideConfig.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbGuideConfig {
  given Arbitrary[GuideConfig] = Arbitrary {
    for {
      tg <- arbitrary[TelescopeGuideConfig]
      gc <- arbitrary[Option[Either[AltairConfig, GemsConfig]]]
    } yield GuideConfig(tg, gc)
  }

  given Cogen[GuideConfig] =
    Cogen[(
      TelescopeGuideConfig,
      Option[Either[AltairConfig, GemsConfig]],
    )].contramap(x => (x.tcsGuide, x.gaosGuide))
}

object ArbGuideConfig extends ArbGuideConfig

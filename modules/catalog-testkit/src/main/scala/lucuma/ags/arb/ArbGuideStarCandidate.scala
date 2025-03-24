// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags.arb

import lucuma.ags.GuideStarCandidate
import lucuma.catalog.BandsList
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue
import lucuma.core.model.SiderealTracking
import lucuma.core.model.arb.ArbSiderealTracking
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbGuideStarCandidate:
  import ArbSiderealTracking.given
  import ArbBrightnessValue.given

  given Arbitrary[GuideStarCandidate] =
    given Arbitrary[Band] = Arbitrary(Gen.oneOf(BandsList.GaiaBandsList.bands))

    Arbitrary:
      for {
        n <- arbitrary[Long]
        t <- arbitrary[SiderealTracking]
        g <- arbitrary[Option[(Band, BrightnessValue)]]
        // This is safe because we have a local Band arbitrary
      } yield GuideStarCandidate.unsafeApply(n, t, g)

  given Cogen[GuideStarCandidate] =
    Cogen[(Long, SiderealTracking, Option[(Band, BrightnessValue)])].contramap(r =>
      (r.id, r.tracking, r.gBrightness)
    )

object ArbGuideStarCandidate extends ArbGuideStarCandidate

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags.arb

import eu.timepit.refined.scalacheck.numeric.*
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.BandsList
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.SiderealTracking
import lucuma.core.model.arb.ArbSiderealTracking
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import scala.collection.immutable.SortedMap

trait ArbGuideStarCandidate:
  import ArbNewType.given
  import ArbRefined.given
  import ArbSiderealTracking.given

  private val candidateBands: List[Band] = (BandsList.GaiaBandsList ∪ BandsList.RBandsList).bands

  given Arbitrary[GuideStarCandidate] =
    Arbitrary:
      for {
        id           <- arbitrary[Long]
        tracking     <- arbitrary[SiderealTracking]
        bands        <- Gen.someOf(candidateBands)
        brightnesses <- Gen.sequence[List[(Band, BrightnessValue)], (Band, BrightnessValue)](
                          bands.toList.map(band => arbitrary[BrightnessValue].map(band -> _))
                        )
      } yield GuideStarCandidate(id, tracking, SortedMap.from(brightnesses))

  given Cogen[GuideStarCandidate] =
    Cogen[(Long, SiderealTracking, List[(Band, BrightnessValue)])].contramap(candidate =>
      (candidate.id, candidate.tracking, candidate.brightnesses.toList)
    )

object ArbGuideStarCandidate extends ArbGuideStarCandidate

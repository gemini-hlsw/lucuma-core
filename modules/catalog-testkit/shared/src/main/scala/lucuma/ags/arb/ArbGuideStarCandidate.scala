// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags.arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarId
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
  import ArbGuideStarId.given
  import ArbNewType.given
  import ArbRefined.given
  import ArbSiderealTracking.given

  private val candidateBands: List[Band] = (BandsList.GaiaBandsList ∪ BandsList.RBandsList).bands

  // Gaia-like values spanning every ranking tier; missing about a quarter of the time
  private val ruweGen: Gen[Option[NonNegBigDecimal]] =
    Gen.frequency(
      1 -> Gen.const(None),
      3 -> Gen
        .choose(0.5, 40.0)
        .map(d =>
          Some(NonNegBigDecimal.unsafeFrom(BigDecimal(d).round(new java.math.MathContext(7))))
        )
    )

  given Arbitrary[GuideStarCandidate] =
    Arbitrary:
      for {
        id           <- arbitrary[GuideStarId]
        tracking     <- arbitrary[SiderealTracking]
        bands        <- Gen.someOf(candidateBands)
        brightnesses <- Gen.sequence[List[(Band, BrightnessValue)], (Band, BrightnessValue)](
                          bands.toList.map(band => arbitrary[BrightnessValue].map(band -> _))
                        )
        ruwe         <- ruweGen
      } yield GuideStarCandidate(id, tracking, SortedMap.from(brightnesses), ruwe)

  given Cogen[GuideStarCandidate] =
    Cogen[(GuideStarId, SiderealTracking, List[(Band, BrightnessValue)], Option[BigDecimal])]
      .contramap(candidate =>
        (candidate.id,
         candidate.tracking,
         candidate.brightnesses.toList,
         candidate.ruwe.map(_.value)
        )
      )

object ArbGuideStarCandidate extends ArbGuideStarCandidate

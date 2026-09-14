// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.kernel.laws.discipline.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.arb.*
import lucuma.catalog.BandsList
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.arb.*
import lucuma.core.optics.laws.discipline.SplitEpiTests
import munit.*

import scala.collection.immutable.SortedMap

class GuideStarCandidateSuite extends DisciplineSuite {
  import ArbGuideStarCandidate.given
  import ArbTarget.given

  // Laws
  checkAll("Eq[GuideStarCandidate]", EqTests[GuideStarCandidate].eqv)
  // optics
  checkAll("GuideStarCandidate.siderealTarget",
           SplitEpiTests(GuideStarCandidate.siderealTarget).splitEpi
  )

  private def target(brightnesses: (Band, Double)*): Target.Sidereal =
    Target.Sidereal(
      NonEmptyString.unsafeFrom("Gaia DR3 123"),
      SiderealTracking.const(Coordinates.Zero),
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          None,
          SortedMap.from(brightnesses.map { case (b, v) =>
            b -> BrightnessValue.unsafeFrom(v).withUnit[VegaMagnitude].toMeasureTagged
          })
        )
      ),
      None
    )

  test("R is estimated from G, BP and RP") {
    // BP - RP = 1.0, G - R = 0.238865
    val candidate = GuideStarCandidate.siderealTarget.get(
      target(Band.Gaia -> 14.5, Band.GaiaBP -> 15.2, Band.GaiaRP -> 14.2)
    )
    assertEqualsDouble(candidate.brightnesses(Band.R).value.value.toDouble, 14.5 - 0.238865, 1e-6)
    assertEquals(candidate.brightnessIn(BandsList.RBandsList).map(_._1), Some(Band.R))
    assertEquals(candidate.brightnessIn(BandsList.GaiaBandsList).map(_._1), Some(Band.GaiaRP))
  }

  test("a catalog R wins over the estimate") {
    val candidate = GuideStarCandidate.siderealTarget.get(
      target(Band.Gaia -> 14.5, Band.GaiaBP -> 15.2, Band.GaiaRP -> 14.2, Band.R -> 13.0)
    )
    assertEquals(candidate.brightnesses.get(Band.R), Some(BrightnessValue.unsafeFrom(13.0)))
  }

  test("no R without a colour") {
    val candidate = GuideStarCandidate.siderealTarget.get(target(Band.Gaia -> 14.5))
    assertEquals(candidate.brightnesses.get(Band.R), None)
    assertEquals(candidate.brightnessIn(BandsList.RBandsList), None)
  }
}

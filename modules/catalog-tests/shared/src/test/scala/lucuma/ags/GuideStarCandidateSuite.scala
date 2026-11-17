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
import lucuma.core.math.dimensional.Measure
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.util.Of
import munit.*

import scala.collection.immutable.SortedMap

class GuideStarCandidateSuite extends DisciplineSuite {
  import ArbGuideStarCandidate.given

  // Laws
  checkAll("Eq[GuideStarCandidate]", EqTests[GuideStarCandidate].eqv)

  private def namedTarget(name: String, brightnesses: (Band, Double)*): Target.Sidereal =
    Target.Sidereal(
      NonEmptyString.unsafeFrom(name),
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

  private def target(brightnesses: (Band, Double)*): Target.Sidereal =
    namedTarget("Gaia DR3 123", brightnesses*)

  test("R is estimated from G, BP and RP") {
    // BP - RP = 1.0, G - R = 0.238865
    val candidate = GuideStarCandidate.fromCatalog(
      target(Band.Gaia -> 14.5, Band.GaiaBP -> 15.2, Band.GaiaRP -> 14.2)
    )
    // Only catalog bands are stored; R is derived on demand
    assertEquals(candidate.brightnesses.keySet, Set[Band](Band.Gaia, Band.GaiaBP, Band.GaiaRP))
    assertEqualsDouble(candidate.rBrightness.get.value.value.toDouble, 14.5 - 0.238865, 1e-6)
    assertEquals(candidate.brightnessIn(BandsList.RBandsList).map(_._1), Some(Band.R))
    assertEquals(candidate.brightnessIn(BandsList.GaiaBandsList).map(_._1), Some(Band.GaiaRP))
  }

  test("a catalog R wins over the estimate") {
    val candidate = GuideStarCandidate.fromCatalog(
      target(Band.Gaia -> 14.5, Band.GaiaBP -> 15.2, Band.GaiaRP -> 14.2, Band.R -> 13.0)
    )
    assertEquals(candidate.rBrightness, Some(BrightnessValue.unsafeFrom(13.0)))
  }

  test("no R without a colour") {
    val candidate = GuideStarCandidate.fromCatalog(target(Band.Gaia -> 14.5))
    assertEquals(candidate.rBrightness, None)
    assertEquals(candidate.brightnessIn(BandsList.RBandsList), None)
  }

  private def point(
    brightnesses: (Band, Measure[BrightnessValue] Of Brightness[Integrated])*
  ): SourceProfile =
    SourceProfile.Point(
      SpectralDefinition.BandNormalized(None, SortedMap.from(brightnesses))
    )

  private val TargetId: Target.Id = Target.Id.fromLong(0x1fL).get

  private val Tracking: SiderealTracking = SiderealTracking.const(Coordinates.Zero)

  test("the observation target keeps Vega and AB magnitudes and drops flux densities") {
    val candidate: GuideStarCandidate = GuideStarCandidate.observationTarget(
      TargetId,
      Tracking,
      point(
        Band.R -> BrightnessValue.unsafeFrom(12.0).withUnit[VegaMagnitude].toMeasureTagged,
        Band.V -> BrightnessValue.unsafeFrom(13.0).withUnit[ABMagnitude].toMeasureTagged,
        Band.J -> BrightnessValue.unsafeFrom(1.0).withUnit[Jansky].toMeasureTagged
      )
    )
    assertEquals(candidate.id, GuideStarId.ObservationTarget(Some(TargetId)))
    assertEquals(
      candidate.brightnesses,
      SortedMap[Band, BrightnessValue](Band.R -> BrightnessValue.unsafeFrom(12.0),
                                       Band.V -> BrightnessValue.unsafeFrom(13.0)
      )
    )
  }

  test("a surface profile gives the observation target no brightnesses") {
    val surface: SourceProfile =
      SourceProfile.Uniform(
        SpectralDefinition.BandNormalized(
          None,
          SortedMap[Band, Measure[BrightnessValue] Of Brightness[Surface]](
            Band.R -> BrightnessValue
              .unsafeFrom(20.0)
              .withUnit[VegaMagnitudePerArcsec2]
              .toMeasureTagged
          )
        )
      )
    assertEquals(
      GuideStarCandidate.observationTarget(TargetId, Tracking, surface).brightnesses.isEmpty,
      true
    )
  }

  test("a catalog star takes its id from its catalog name") {
    val candidate: GuideStarCandidate = GuideStarCandidate.fromCatalog(target())
    assertEquals(candidate.id, GuideStarId.GaiaDr3(123L))
    assertEquals(candidate.catalogName.map(_.value.value), Some("Gaia DR3 123"))
    assertEquals(candidate.id.gaiaSourceId, Some(123L))
  }

  test("a catalog star with another name gets gaia(-1)") {
    val candidate: GuideStarCandidate =
      GuideStarCandidate.fromCatalog(namedTarget("HD 1234"))
    assertEquals(candidate.id, GuideStarId.GaiaDr3(-1L))
  }

  test("the observation target has its target id and no catalog name") {
    val candidate: GuideStarCandidate =
      GuideStarCandidate.observationTarget(TargetId, Tracking, point())
    assertEquals(candidate.id, GuideStarId.ObservationTarget(Some(TargetId)))
    assertEquals(candidate.catalogName, None)
  }
}

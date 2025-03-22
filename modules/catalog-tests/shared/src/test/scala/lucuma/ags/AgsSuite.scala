// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.ags.AgsAnalysis.NoMagnitudeForBand
import lucuma.ags.AgsAnalysis.Usable
import lucuma.ags.AgsAnalysis.VignettesScience
import lucuma.core.enums.*
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.SiderealTracking

class AgsSuite extends munit.FunSuite {
  val gs1 = GuideStarCandidate.unsafeApply(0L,
                                           SiderealTracking.const(Coordinates.Zero),
                                           (Band.Gaia, BrightnessValue.unsafeFrom(16.05)).some
  )

  val gs2 = GuideStarCandidate.unsafeApply(1L,
                                           SiderealTracking.const(Coordinates.Zero),
                                           (Band.Gaia, BrightnessValue.unsafeFrom(11.23)).some
  )

  extension (l: Long) def toArea: Area = Area.fromMicroarcsecondsSquared.getOption(l).get

  extension (usable: Usable)
    def slow: Usable                = usable.copy(guideSpeed = GuideSpeed.Slow)
    def possibleDegradation: Usable = usable.copy(quality = AgsGuideQuality.PossibleIqDegradation)
    def vignetting(v: Long): Usable = usable.copy(vignetting = v.toArea)

  def usable(
    gs:         GuideStarCandidate,
    posAngle:   Angle,
    vignetting: Long = 0,
    speed:      GuideSpeed = GuideSpeed.Fast,
    quality:    AgsGuideQuality = AgsGuideQuality.DeliversRequestedIq
  ): Usable = Usable(GuideProbe.GmosOIWFS, gs, speed, quality, posAngle, vignetting.toArea)

  val u1a = usable(gs1, Angle.Angle0)
  val u1b = usable(gs1, Angle.Angle180)
  val u2a = usable(gs2, Angle.Angle0)
  val u2b = usable(gs2, Angle.Angle180)

  test("usable comparisons") {
    // less vignetting wins
    assert(Usable.rankOrdering.compare(u1a.vignetting(1), u1a) > 0)
    // same vignetting but brighter wins
    assert(Usable.rankOrdering.compare(u1a, u2a) > 0)
    // vignetting beats brighteness
    assert(Usable.rankOrdering.compare(u1a, u2a.vignetting(1)) < 0)
    // guide speed beats vignetting
    assert(Usable.rankOrdering.compare(u1a.slow, u1a.vignetting(1)) > 0)
    // quality beats vignetting
    assert(Usable.rankOrdering.compare(u1a.possibleDegradation, u1a.vignetting(1)) > 0)
  }

  test("sortUsablePositions - bad angles thrown out") {
    val noMag            = NoMagnitudeForBand(GuideProbe.GmosOIWFS, gs1, Angle.Angle0)
    val vignettesScience = VignettesScience(gs2, AgsPosition(Angle.Angle180, Offset.Zero))
    val analyses         = List(noMag, u1a, u1b.vignetting(9), u2a.vignetting(5), u2b, vignettesScience)
    val sorted           = analyses.sortUsablePositions
    assertEquals(sorted, List(u2a.vignetting(5), u1b.vignetting(9)))
  }

  test("sortUsablePositions - more complex") {
    // in real life, the Usable instances should be the same except for vignetting and posAngle,
    // but this test ensures that the sorting works correctly in case the algorithm changes

    // worst for angle is chosen
    val vignette7 = u1a.vignetting(7)
    val vignette9 = u1a.vignetting(9)
    // but vignetting beats slow
    val slow      = u1b.slow
    // vignetting also beats quality
    val degraded  = u2a.possibleDegradation
    // least bad vignetting wins
    val vignetteB = u2b.vignetting(8)

    val analyses = List(vignette7, vignette9, slow, degraded, vignetteB)
    val sorted   = analyses.sortUsablePositions
    assertEquals(sorted, List(vignetteB, vignette9))
  }

  test("sortUsablePositions - all else being equal, brightness breaks the tie") {
    val a        = u1a.vignetting(7).slow.possibleDegradation
    val b        = u2a.vignetting(7).slow.possibleDegradation
    val analyses = List(a, b)
    val sorted   = analyses.sortUsablePositions
    assertEquals(sorted, List(b, a))
  }

  test("discard science target") {
    val constraints = ConstraintSet(ImageQuality.PointTwo,
                                    CloudExtinction.PointFive,
                                    SkyBackground.Dark,
                                    WaterVapor.Wet,
                                    ElevationRange.AirMass.Default
    )

    val wavelength = Wavelength.fromIntNanometers(300).get

    assertEquals(
      Ags
        .agsAnalysis(
          constraints,
          wavelength,
          Coordinates.Zero,
          List(Coordinates.Zero),
          NonEmptyList.of(AgsPosition(Angle.Angle0, Offset.Zero)),
          AgsParams.GmosAgsParams(GmosNorthFpu.LongSlit_5_00.asLeft.some, PortDisposition.Bottom),
          List(gs1)
        )
        .headOption,
      AgsAnalysis.VignettesScience(gs1, AgsPosition(Angle.Angle0, Offset.Zero)).some
    )

    val gsOffset =
      GuideStarCandidate.unsafeApply(
        0L,
        SiderealTracking.const(
          Coordinates.Zero
            .offsetBy(Angle.Angle0, Offset.signedDecimalArcseconds.reverseGet(0.0, 23.0))
            .get
        ),
        (Band.Gaia, BrightnessValue.unsafeFrom(15)).some
      )

    assert(
      Ags
        .agsAnalysis(
          constraints,
          wavelength,
          Coordinates.Zero,
          Nil,
          NonEmptyList.of(AgsPosition(Angle.Angle0, Offset.Zero)),
          AgsParams.GmosAgsParams(GmosNorthFpu.LongSlit_5_00.asLeft.some, PortDisposition.Bottom),
          List(gsOffset)
        )
        .headOption
        .forall(_.isUsable)
    )
  }
}

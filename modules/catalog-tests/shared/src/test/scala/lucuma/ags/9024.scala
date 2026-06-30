// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.all.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.AgsAnalysis.VignettesScience
import lucuma.ags.ScienceOffsets
import lucuma.ags.syntax.*
import lucuma.core.enums.Band
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Offset
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.int.*
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.SiderealTracking

// Regression tests for the GHOST ifu vignetting
class ShortCut_9024 extends munit.FunSuite {

  private val constraints = ConstraintSet(
    ImageQuality.Preset.PointTwo,
    CloudExtinction.Preset.PointFive,
    SkyBackground.Dark,
    WaterVapor.Wet,
    ElevationRange.ByAirMass.Default
  )

  private val wavelength = Wavelength.fromIntNanometers(300).get

  private def coordAt(offset: Offset): Coordinates =
    Coordinates.Zero.offsetBy(Angle.Angle0, offset).get

  private def guidestarAt(offset: Offset): GuideStarCandidate =
    GuideStarCandidate(
      0L,
      SiderealTracking.const(coordAt(offset)),
      (Band.Gaia, BrightnessValue.unsafeFrom(12.0)).some
    ).get

  private def ghostAnalysis(
    gs:             GuideStarCandidate,
    scienceTargets: List[Coordinates],
    blindOffset:    Option[Coordinates]
  ): List[AgsAnalysis] =
    Ags
      .agsAnalysis(
        constraints,
        wavelength,
        Coordinates.Zero,
        scienceTargets,
        blindOffset,
        NonEmptyList.of(Angle.Angle0),
        Some(AcquisitionOffsets(NonEmptySet.of(Offset.Zero.guided))),
        Some(ScienceOffsets(NonEmptySet.of(Offset.Zero.guided))),
        AgsParams.GhostIfu(),
        List(gs)
      )
      .analyses

  private def vignettes(as: List[AgsAnalysis]): Boolean =
    as.exists:
      case VignettesScience(_, _) => true
      case _                      => false

  // target like one on ifu1/ifu2 that is away from the base
  private val noZoneTarget = coordAt(Offset(-100.arcsec.p, 0.arcsec.q))
  // This guide star is on top of ifu1/ifu2 it should be reject
  private val gsOnIFU      = guidestarAt(Offset(100.arcsec.p, -100.arcsec.q))
  // This guide star is away
  private val gsAtDistance = guidestarAt(Offset(100.arcsec.p, 0.arcsec.q))

  test("use the probe arm at the guide star location") {
    val vignetting   = ghostAnalysis(gsOnIFU, List(noZoneTarget), None)
    val noVignetting = ghostAnalysis(gsAtDistance, List(noZoneTarget), None)

    assert(vignettes(vignetting))
    assert(!vignettes(noVignetting))
    assert(noVignetting.exists(_.isUsable))
  }

  test("a blind offset is protected from probe vignetting") {
    val vignetting = ghostAnalysis(gsOnIFU, Nil, noZoneTarget.some)
    assert(vignettes(vignetting))
  }

  private def coord(ra: String, dec: String): Coordinates =
    Coordinates(
      RightAscension.fromStringHMS.getOption(ra).get,
      Declination.fromStringSignedDMS.getOption(dec).get
    )

  private def gaiaCandidate(id: Long, ra: String, dec: String, g: Double): GuideStarCandidate =
    GuideStarCandidate(
      id,
      SiderealTracking.const(coord(ra, dec)),
      (Band.GaiaRP, BrightnessValue.unsafeFrom(g)).some
    ).get

  test("selects a clean guide star over a brighter vignetting one") {
    val realConstraints = ConstraintSet(
      ImageQuality.Preset.OnePointZero,
      CloudExtinction.Preset.PointThree,
      SkyBackground.Bright,
      WaterVapor.Wet,
      ElevationRange.ByAirMass.Default
    )
    val realWavelength  = Wavelength.fromIntNanometers(900).get

    val offendingId = 5145821452872321792L
    val cleanId     = 5145828496618683520L

    val t1   = coord("02:27:53.351220", "-15:15:31.073879")
    val t2   = coord("02:27:32.351218", "-15:14:29.073879")
    val base = Coordinates.centerOf(NonEmptyList.of(t1, t2))

    val offending = gaiaCandidate(offendingId, "02:27:23.401952", "-15:19:24.661144", 12.623376)
    val clean     = gaiaCandidate(cleanId, "02:27:33.164983", "-15:08:44.481449", 13.580871)

    val analyses = Ags
      .agsAnalysis(
        realConstraints,
        realWavelength,
        base,
        List(t1, t2),
        None,
        NonEmptyList.of(Angle.Angle0),
        None,
        None,
        AgsParams.GhostIfu(),
        List(offending, clean)
      )
      .analyses

    // The offending star's probe vignettes the second IFU target.
    assert(analyses.filter(_.target.id === offendingId).exists {
      case VignettesScience(_, _) => true
      case _                      => false
    })

    // The clean star is the one selected.
    assertEquals(analyses.sortUsablePositions.headOption.map(_.target.id), cleanId.some)
  }
}

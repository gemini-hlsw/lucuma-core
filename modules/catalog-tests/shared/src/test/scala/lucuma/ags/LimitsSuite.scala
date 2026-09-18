// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import lucuma.catalog.BandsList
import lucuma.catalog.BrightnessConstraints
import lucuma.catalog.FaintnessConstraint
import lucuma.catalog.SaturationConstraint
import lucuma.core.enums.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality

class LimitsSuite extends munit.FunSuite {
  test("widestConstraints") {
    assertEquals(
      widestConstraints,
      BrightnessConstraints(
        BandsList.GaiaBandsList,
        FaintnessConstraint(BrightnessValue.unsafeFrom(17.527769563884206)),
        None
      )
    )
  }

  test("gaia brightness constraints") {
    // Sample query from explore

    val constraints = ConstraintSet(
      ImageQuality.Preset.PointOne,    // min image quality
      CloudExtinction.Preset.PointOne, // min cloud extinction
      SkyBackground.Dark,              // Not relevant
      WaterVapor.Wet,                  // Not relevant
      ElevationRange.ByAirMass.Default // Not relevant
    )

    val wavelength = Wavelength.fromIntNanometers(300).get

    val brightnessConstraints =
      gaiaBrightnessConstraints(constraints, GuideProbe.GmosOIWFS, GuideSpeed.Fast, wavelength)

    assertEquals(
      brightnessConstraints,
      BrightnessConstraints(
        BandsList.GaiaBandsList,
        FaintnessConstraint(BrightnessValue.unsafeFrom(16.227769563884202)),
        Some(SaturationConstraint(BrightnessValue.unsafeFrom(10.227769563884202)))
      )
    )
  }

  // IQ 0.2" is in the 20% bucket, 2.0" in the "any" bucket, at any wavelength
  private def altairConstraints(iq: ImageQuality.Preset, sb: SkyBackground): ConstraintSet =
    ConstraintSet(
      iq,
      CloudExtinction.Preset.PointOne,
      sb,
      WaterVapor.Wet,
      ElevationRange.ByAirMass.Default
    )

  private val wavelength: Wavelength = Wavelength.fromIntNanometers(1650).get

  test("altair NGS limits are the OCS R table minus cloud extinction") {
    val limits = guideStarBrightnessConstraints(
      altairConstraints(ImageQuality.Preset.PointTwo, SkyBackground.Dark),
      GuideProbe.AltairAOWFS,
      Some(AltairMode.Ngs),
      GuideSpeed.Slow,
      wavelength
    )
    // IQ20 / SB50 slow: 15.05, CE 0.1 mag, saturation 17 mag brighter
    assertEquals(limits.searchBands, BandsList.RBandsList)
    assertEqualsDouble(limits.faintnessConstraint.brightness.value.value.toDouble, 14.95, 1e-6)
    assertEqualsDouble(
      limits.saturationConstraint.get.brightness.value.value.toDouble,
      14.95 - 17.0,
      1e-6
    )
  }

  test("altair LGS limits are fainter and saturate sooner") {
    val limits = guideStarBrightnessConstraints(
      altairConstraints(ImageQuality.Preset.PointTwo, SkyBackground.Dark),
      GuideProbe.AltairAOWFS,
      Some(AltairMode.Lgs),
      GuideSpeed.Fast,
      wavelength
    )
    // IQ20 / SB50 fast: 15.80, CE 0.1 mag, saturation 5 mag brighter
    assertEqualsDouble(limits.faintnessConstraint.brightness.value.value.toDouble, 15.70, 1e-6)
    assertEqualsDouble(
      limits.saturationConstraint.get.brightness.value.value.toDouble,
      15.70 - 5.0,
      1e-6
    )
  }

  test("altair limits follow the image quality and sky background buckets") {
    val worst = guideStarBrightnessConstraints(
      altairConstraints(ImageQuality.Preset.TwoPointZero, SkyBackground.Bright),
      GuideProbe.AltairAOWFS,
      Some(AltairMode.Ngs),
      GuideSpeed.Medium,
      wavelength
    )
    // ANY / ANY medium: 12.50, CE 0.1 mag
    assertEqualsDouble(worst.faintnessConstraint.brightness.value.value.toDouble, 12.40, 1e-6)
  }

  test("LGS+P1 guides on PWFS1 with the Gaia limits") {
    val constraints = altairConstraints(ImageQuality.Preset.PointTwo, SkyBackground.Dark)
    assertEquals(
      guideStarBrightnessConstraints(
        constraints,
        AltairMode.LgsP1.guideProbe,
        Some(AltairMode.LgsP1),
        GuideSpeed.Fast,
        wavelength
      ),
      gaiaBrightnessConstraints(constraints, GuideProbe.PWFS1, GuideSpeed.Fast, wavelength)
    )
  }
}

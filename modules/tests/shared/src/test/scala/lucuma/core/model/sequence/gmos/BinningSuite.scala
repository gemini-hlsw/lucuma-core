// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.syntax.option.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.sequence.gmos.binning.*
import munit.FunSuite

import scala.collection.immutable.SortedMap

class BinningSuite extends FunSuite:

  def bandNormalized[T]: BandNormalized[T] =
    BandNormalized(none, SortedMap.empty)

  //  Examples with R831
  //  slit=0.50, fwhm=0.6: 1 2
  //  slit=0.75, fwhm=1.0: 2 4
  //  slit=1.00, fwhm=1.5: 2 4

  private def testLongslit(
    fpu:        GmosNorthFpu,
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    grating:    GmosNorthGrating,
    expectX:    GmosXBinning,
    expectY:    GmosYBinning
  ): Unit = {
    val xy = longslit.northBinning(fpu, srcProfile, iq, grating)
    assertEquals(xy, (expectX, expectY))
  }

  test("longslit, R831, slit=0.50, fwhm=0.6: 1 2"):
    testLongslit(
      GmosNorthFpu.LongSlit_0_50,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(600_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.One,
      GmosYBinning.Two
    )

  test("longslit, R831, slit=0.75, fwhm=1.0: 2 2"):
    testLongslit(
      GmosNorthFpu.LongSlit_0_75,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_000_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("longslit, R831, slit=1.00, fwhm=1.5: 2 2"):
    testLongslit(
      GmosNorthFpu.LongSlit_1_00,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_500_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("longslit, R831, slit=0.50, Uniform:  2 2"):
    testLongslit(
      GmosNorthFpu.LongSlit_0_50,
      SourceProfile.Uniform(bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.One,
      GmosYBinning.Two
    )

  // Testing explore-dev using GMOS-N with the R831 grating with a 1" slit and IQ=1.0" should yield binning = 2 x 4,
  // however, the sequence displays 1 x 1.
  test("shortcut-2772"):
    testLongslit(
      GmosNorthFpu.LongSlit_1_00,
      SourceProfile.Point(bandNormalized),
      ImageQuality.Preset.OnePointZero.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("longslit, R831, slit=0.50, fwhm=0.0: 1 1"):
    testLongslit(
      GmosNorthFpu.LongSlit_0_50,
      SourceProfile.Gaussian(Angle.Angle0, bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.One,
      GmosYBinning.One
    )

  test("longslit, B480, slit=0.55, fwhm=1.0: edge case for spectral binning"):
    testLongslit(
      GmosNorthFpu.LongSlit_0_50, // Using closest available slit
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_000_000L), bandNormalized),
      ImageQuality.Preset.OnePointZero.toImageQuality,
      GmosNorthGrating.B480_G5309,
      GmosXBinning.One,
      GmosYBinning.Two
    )

  test("longslit, B480, slit=0.75, fwhm=1.0: edge case for spectral binning = 2"):
    testLongslit(
      GmosNorthFpu.LongSlit_0_75,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_000_000L), bandNormalized),
      ImageQuality.Preset.OnePointZero.toImageQuality,
      GmosNorthGrating.B480_G5309,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("longslit, B480, slit=1.00, fwhm=0.6: mixed case"):
    testLongslit(
      GmosNorthFpu.LongSlit_1_00,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(600_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.B480_G5309,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("spatial binning edge cases"):
    val profile = SourceProfile.Point(bandNormalized)

    // Just below threshold for bin=2: fwhm=0.40 -> bin=1
    assertEquals(
      spatialBinning(profile, ImageQuality.unsafeFromArcSeconds(0.40), DefaultGmosNorthDetector.pixelSize, DefaultMaxYBinning, DefaultSampling),
      GmosYBinning.One
    )

    // Just above threshold for bin=2: fwhm=0.41 -> bin=2
    assertEquals(
      spatialBinning(profile, ImageQuality.unsafeFromArcSeconds(0.41), DefaultGmosNorthDetector.pixelSize, DefaultMaxYBinning, DefaultSampling),
      GmosYBinning.Two
    )

    // Just below threshold for bin=4: fwhm=0.80 -> bin=2
    assertEquals(
      spatialBinning(profile, ImageQuality.unsafeFromArcSeconds(0.80), DefaultGmosNorthDetector.pixelSize, DefaultMaxYBinning, DefaultSampling),
      GmosYBinning.Two
    )

    // Just above threshold for bin=4: fwhm=0.81 -> bin=2 (capped)
    assertEquals(
      spatialBinning(profile, ImageQuality.unsafeFromArcSeconds(0.81), DefaultGmosNorthDetector.pixelSize, DefaultMaxYBinning, DefaultSampling),
      GmosYBinning.Two
    )

  private def testImaging(
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    expectX:    GmosXBinning,
    expectY:    GmosYBinning
  ): Unit = {
    val xy = imaging.northBinning(srcProfile, iq)
    assertEquals(xy, (expectX, expectY))
  }

  test("imaging, fwhm=0.40: 1x1"):
    testImaging(
      SourceProfile.Point(bandNormalized),
      ImageQuality.unsafeFromArcSeconds(0.40),
      GmosXBinning.One,
      GmosYBinning.One
    )

  test("imaging, fwhm=0.41: 2x2"):
    testImaging(
      SourceProfile.Point(bandNormalized),
      ImageQuality.unsafeFromArcSeconds(0.41),
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("imaging, fwhm=0.81: 2x2 (capped)"):
    testImaging(
      SourceProfile.Point(bandNormalized),
      ImageQuality.unsafeFromArcSeconds(0.81),
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("imaging, Gaussian fwhm=0.6: 2x2"):
    testImaging(
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(600_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("imaging, Uniform source: 2x2"):
    testImaging(
      SourceProfile.Uniform(bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosXBinning.Two,
      GmosYBinning.Two
    )

  test("mos, B480, slit=1.00, fwhm=1.12: 2x2"):
    // calc: effective_width = min(1.00" slit, 1.584" object_size) = 1.00"
    // npix = 422 / (1520*0.5/1.00) / 0.062 = 8.956, npix/4 = 2.24 < 2.5 -> bin=2
    val xy = mos.northBinning(
      GmosNorthFpu.LongSlit_1_00,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_120_000L), bandNormalized),
      ImageQuality.unsafeFromArcSeconds(1.12), // 1.12" seeing
      GmosNorthGrating.B480_G5309)
    assertEquals(xy, (GmosXBinning.Two, GmosYBinning.Two))

  test("mos vs longslit spatial binning constraint"):
    // Test that MOS limits spatial binning to 2x while longslit allows 4x
    val profile = SourceProfile.Point(bandNormalized)
    val iq = ImageQuality.unsafeFromArcSeconds(1.0) // Would normally give 4x binning

    // Longslit constrains spatial binning to 2x maximum
    val longslitResult = spatialBinning(profile, iq, DefaultGmosNorthDetector.pixelSize, DefaultMaxYBinning, DefaultSampling)
    assertEquals(longslitResult, GmosYBinning.Two)

    // MOS constrains spatial binning to 2x maximum
    val mosResult = mos.mosSpatialBinning(profile, iq, DefaultGmosNorthDetector.pixelSize)
    assertEquals(mosResult, GmosYBinning.Two)

  test("mos spatial binning within constraint"):
    // Test case where spatial binning is naturally ≤2, so no constraint applied
    val profile = SourceProfile.Point(bandNormalized)
    val iq = ImageQuality.unsafeFromArcSeconds(0.5) // Should give 2x binning naturally

    val mosResult = mos.mosSpatialBinning(profile, iq, DefaultGmosNorthDetector.pixelSize)
    assertEquals(mosResult, GmosYBinning.Two)

  test("mos spatial binning no constraint needed"):
    // Test case where spatial binning is naturally 1x
    val profile = SourceProfile.Point(bandNormalized)
    val iq = ImageQuality.unsafeFromArcSeconds(0.3) // Should give 1x binning

    val mosResult = mos.mosSpatialBinning(profile, iq,
      lucuma.core.enums.GmosNorthDetector.Hamamatsu.pixelSize)
    assertEquals(mosResult, GmosYBinning.One)

  test("mos vs longslit comparison with same parameters"):
    // Compare MOS and longslit with identical parameters to show the Y-binning constraint
    val fpu = GmosNorthFpu.LongSlit_1_00
    val profile = SourceProfile.Point(bandNormalized)
    val iq = ImageQuality.unsafeFromArcSeconds(1.0)
    val grating = GmosNorthGrating.R831_G5302

    val longslitResult = longslit.northBinning(fpu, profile, iq, grating)
    val mosResult = mos.northBinning(fpu, profile, iq, grating)

    assertEquals(longslitResult._1, mosResult._1)

    // MOS and longslit should limit spatial binning to 2x
    assertEquals(longslitResult._2, GmosYBinning.Two)
    assertEquals(mosResult._2, GmosYBinning.Two)

  test("ifu always returns 1x1"):
    val xy = ifu.ifuBinning
    assertEquals(xy, (GmosXBinning.One, GmosYBinning.One))


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
import munit.FunSuite

import scala.collection.immutable.SortedMap

final class BinningSuite extends FunSuite {

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

  test("longslit, R831, slit=0.50, fwhm=0.6: 1 2") {
    testLongslit(
      GmosNorthFpu.LongSlit_0_50,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(600_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.One,
      GmosYBinning.Two
    )
  }

  test("longslit, R831, slit=0.75, fwhm=1.0: 2 4") {
    testLongslit(
      GmosNorthFpu.LongSlit_0_75,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_000_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.Two,
      GmosYBinning.Four
    )
  }

  test("longslit, R831, slit=1.00, fwhm=1.5: 2 4") {
    testLongslit(
      GmosNorthFpu.LongSlit_1_00,
      SourceProfile.Gaussian(Angle.microarcseconds.reverseGet(1_500_000L), bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.Two,
      GmosYBinning.Four
    )
  }

  test("longslit, R831, slit=0.50, Uniform:  2 4") {
    testLongslit(
      GmosNorthFpu.LongSlit_0_50,
      SourceProfile.Uniform(bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.One,
      GmosYBinning.Four
    )
  }

  // Testing explore-dev using GMOS-N with the R831 grating with a 1" slit and IQ=1.0" should yield binning = 2 x 4,
  // however, the sequence displays 1 x 1.
  test("shortcut-2772") {
    testLongslit(
      GmosNorthFpu.LongSlit_1_00,
      SourceProfile.Point(bandNormalized),
      ImageQuality.Preset.OnePointZero.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.Two,
      GmosYBinning.Four
    )
  }

  test("longslit, R831, slit=0.50, fwhm=0.0: 1 1") {
    testLongslit(
      GmosNorthFpu.LongSlit_0_50,
      SourceProfile.Gaussian(Angle.Angle0, bandNormalized),
      ImageQuality.Preset.PointOne.toImageQuality,
      GmosNorthGrating.R831_G5302,
      GmosXBinning.One,
      GmosYBinning.One
    )
  }

}

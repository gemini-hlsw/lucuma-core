// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Order
import cats.syntax.all.*
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.util.Enumerated
import spire.math.Rational

import java.math.RoundingMode.HALF_UP

/**
 * Optimal GMOS spectral and spatial binning calculations.
 *
 * See https://docs.google.com/document/d/1P8_pXLRVomUSvofyVkAniOyGROcAtiJ7EMYt9wWXB0o.
 */
object binning {

  val DefaultSampling: PosDouble =
    PosDouble.unsafeFrom(2.5)

  val DefaultGmosSouthDetector: GmosSouthDetector =
    GmosSouthDetector.Hamamatsu

  val DefaultGmosNorthDetector: GmosNorthDetector =
    GmosNorthDetector.Hamamatsu

  /**
   * Object angular size estimate based on source profile and image quality.
   */
  def objectSize(p: SourceProfile, iq: ImageQuality): Angle =
    p match {
      case SourceProfile.Point(_)          =>
        iq.toAngle

      case SourceProfile.Uniform(_)        =>
        Angle.Angle180

      case SourceProfile.Gaussian(fwhm, _) =>
        val a = fwhm.toSignedDoubleDegrees
        val b = iq.toAngle.toSignedDoubleDegrees
        Angle.fromDoubleDegrees(Math.sqrt(a*a + b*b))
    }

  extension (λ: Angle) {
    def arcsec: BigDecimal =
      Angle.decimalArcseconds.get(λ)
  }

  /**
   * Minimum of the slit and object size.
   */
  def effectiveWidth(
    slitWidth:  Angle,
    srcProfile: SourceProfile,
    iq:         ImageQuality
  ): Angle = {
    given Order[Angle] = Angle.AngleOrder
    slitWidth min objectSize(srcProfile, iq)
  }

  def spectralBinning(
    slitWidth:  Angle,
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    dispersion: Quantity[Rational, NanometersPerPixel],
    resolution: PosInt,
    blaze:      Wavelength,
    sampling:   PosDouble = DefaultSampling
  ): GmosXBinning = {

    // Effective resolution of slit
    val effRes = resolution.value.toDouble / 2.0 / effectiveWidth(slitWidth, srcProfile, iq).arcsec

    // Unbinned sampling
    val nPix   = blaze.toNanometers.value.value/effRes/dispersion.value.toBigDecimal(4, HALF_UP)

    // The maximum binning that gives the required sampling (<npix)
    Enumerated[GmosXBinning].all.tail.reverse.find { bin =>
      sampling.value < (nPix / bin.count.value)
    }.getOrElse(GmosXBinning.One)
  }

  def spatialBinning(
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    pixelScale: Angle,
    sampling:   PosDouble = DefaultSampling
  ): GmosYBinning = {

    // Number of unbinned pixels to sample the slit width
    val npix = objectSize(srcProfile, iq).arcsec / (pixelScale.arcsec * sampling.value)

    // The maximum binning that gives the required sampling (<npix)
    Enumerated[GmosYBinning].all.tail.reverse.find { bin =>
      bin.count.value <= npix
    }.getOrElse(GmosYBinning.One)
  }

  /**
   * Optimal GMOS binning calculation for imaging mode.
   * For imaging, both X and Y binning are the same (spatial binning).
   */
  def imagingBinning(
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    pixelScale: Angle,
    sampling:   PosDouble = DefaultSampling
  ): (GmosXBinning, GmosYBinning) = {
    val yBin = spatialBinning(srcProfile, iq, pixelScale, sampling)
    (GmosXBinning(yBin.value), yBin)
  }

  /**
   * Spatial binning for MOS mode with maximum binning constraint.
   */
  def mosSpatialBinning(
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    pixelScale: Angle,
    maxBinning: GmosYBinning = mos.DefaultMaxYBinning,
    sampling:   PosDouble    = DefaultSampling
  ): GmosYBinning =
    maxBinning.min(spatialBinning(srcProfile, iq, pixelScale, sampling))

  /**
   * Optimal GMOS binning calculation for MOS (Multi-Object Spectroscopy) mode.
   * Uses spectral binning for X-axis and constrained spatial binning for Y-axis.
   */
  def mosBinning(
    slitWidth:  Angle,
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    dispersion: Quantity[Rational, NanometersPerPixel],
    resolution: PosInt,
    blaze:      Wavelength,
    pixelScale: Angle,
    maxYBin:    GmosYBinning = mos.DefaultMaxYBinning,
    sampling:   PosDouble    = DefaultSampling
  ): (GmosXBinning, GmosYBinning) = {
    val xBin = spectralBinning(slitWidth, srcProfile, iq, dispersion, resolution, blaze, sampling)
    val yBin = mosSpatialBinning(srcProfile, iq, pixelScale, maxYBin, sampling)
    (xBin, yBin)
  }

  /**
   * Optimal GMOS binning calculation for IFU (Integral Field Unit) mode.
   * IFU observations always use 1x1 binning to maintain spatial resolution
   * required for proper reconstruction of the integral field.
   */
  val ifuBinning: (GmosXBinning, GmosYBinning) =
    (GmosXBinning.One, GmosYBinning.One)

}

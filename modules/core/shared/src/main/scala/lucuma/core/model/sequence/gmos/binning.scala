// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Order
import cats.syntax.all.*
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosDouble
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
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
      sampling.value < (nPix / bin.count)
    }.getOrElse(GmosXBinning.One)
  }

  def northSpectralBinning(
    fpu:        GmosNorthFpu,
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    grating:    GmosNorthGrating,
    sampling:   PosDouble = DefaultSampling
  ): GmosXBinning =
    spectralBinning(fpu.effectiveSlitWidth, srcProfile, iq, grating.dispersion, grating.referenceResolution, grating.blazeWavelength, sampling)

  def southSpectralBinning(
    fpu:        GmosSouthFpu,
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    grating:    GmosSouthGrating,
    sampling:   PosDouble = DefaultSampling
  ): GmosXBinning =
    spectralBinning(fpu.effectiveSlitWidth, srcProfile, iq, grating.dispersion, grating.referenceResolution, grating.blazeWavelength, sampling)

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
      bin.count <= npix
    }.getOrElse(GmosYBinning.One)
  }

  def northSpatialBinning(
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    detector:   GmosNorthDetector = GmosNorthDetector.Hamamatsu,
    sampling:   PosDouble         = DefaultSampling
  ): GmosYBinning =
    spatialBinning(srcProfile, iq, detector.pixelSize, sampling)

  def southSpatialBinning(
    srcProfile: SourceProfile,
    iq:         ImageQuality,
    detector:   GmosSouthDetector = GmosSouthDetector.Hamamatsu,
    sampling:   PosDouble         = DefaultSampling
  ): GmosYBinning =
    spatialBinning(srcProfile, iq, detector.pixelSize, sampling)

}

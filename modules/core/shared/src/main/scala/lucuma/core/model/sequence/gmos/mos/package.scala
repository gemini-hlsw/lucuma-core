// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package mos

import cats.data.NonEmptyList
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
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.TelescopeConfig
import spire.math.Rational

/**
 * MOS positions are full p/q offsets rather than slit telescope configs.
 *
 * The default does not nod at all.
 */
val DefaultTelescopeConfigs: NonEmptyList[TelescopeConfig] =
  NonEmptyList.one(TelescopeConfig(Offset.Zero, StepGuideState.Enabled))

/**
  * Spatial binning for MOS mode with maximum binning constraint.
  */
def mosSpatialBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  pixelScale: Angle,
  maxBinning: GmosYBinning = binning.DefaultMaxYBinning,
  sampling:   PosDouble    = binning.DefaultSampling
): GmosYBinning =
  binning.spatialBinning(srcProfile, iq, pixelScale, maxBinning, sampling)

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
  maxYBin:    GmosYBinning = binning.DefaultMaxYBinning,
  sampling:   PosDouble    = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) = {
  val xBin = binning.spectralBinning(slitWidth, srcProfile, iq, dispersion, resolution, blaze, sampling)
  val yBin = mosSpatialBinning(srcProfile, iq, pixelScale, maxYBin, sampling)
  (xBin, yBin)
}

/**
 * Optimal GMOS binning calculation for MOS (Multi-Object Spectroscopy).
 */
def northBinning(
  fpu:        GmosNorthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosNorthGrating,
  detector:   GmosNorthDetector = binning.DefaultGmosNorthDetector,
  maxYBin:    GmosYBinning      = binning.DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  mosBinning(
    fpu.effectiveSlitWidth,
    srcProfile,
    iq,
    grating.dispersion,
    grating.referenceResolution,
    grating.blazeWavelength,
    detector.pixelSize,
    maxYBin,
    sampling
  )

/**
 * Optimal GMOS binning calculation for MOS (Multi-Object Spectroscopy).
 */
def southBinning(
  fpu:        GmosSouthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosSouthGrating,
  detector:   GmosSouthDetector = binning.DefaultGmosSouthDetector,
  maxYBin:    GmosYBinning      = binning.DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  mosBinning(
    fpu.effectiveSlitWidth,
    srcProfile,
    iq,
    grating.dispersion,
    grating.referenceResolution,
    grating.blazeWavelength,
    detector.pixelSize,
    maxYBin,
    sampling
  )

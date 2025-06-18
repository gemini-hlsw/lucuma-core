// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package mos

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
import spire.math.Rational

/**
 * Default maximum Y binning for MOS mode.
 * MOS observations typically use 2x2 binning maximum for spatial direction
 * to maintain adequate sampling for object identification and positioning.
 */
val DefaultMaxYBinning: GmosYBinning =
  GmosYBinning.Two

/**
  * Spatial binning for MOS mode with maximum binning constraint.
  */
def mosSpatialBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  pixelScale: Angle,
  maxBinning: GmosYBinning = DefaultMaxYBinning,
  sampling:   PosDouble    = binning.DefaultSampling
): GmosYBinning =
  maxBinning.min(binning.spatialBinning(srcProfile, iq, pixelScale, sampling))

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
  maxYBin:    GmosYBinning = DefaultMaxYBinning,
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
  maxYBin:    GmosYBinning      = DefaultMaxYBinning,
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
  maxYBin:    GmosYBinning      = DefaultMaxYBinning,
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

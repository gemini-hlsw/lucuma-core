// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package imaging

import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile

/**
 * Default MOS pre-imaging offsets, in arcseconds: (0, 0), (-4, -6), (4, -6), (8, 0).
 */
val DefaultPreImagingOffsets: (Offset, Offset, Offset, Offset) =
  (
    Offset.Zero,
    Offset(-(4.pArcsec), -(6.qArcsec)),
    Offset(  4.pArcsec,  -(6.qArcsec)),
    Offset(  8.pArcsec,    0.qArcsec)
  )

/**
  * Optimal GMOS binning calculation for imaging mode.
  * For imaging, both X and Y binning are the same (spatial binning).
  */
def imagingBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  pixelScale: Angle,
  maxBinning: GmosYBinning = binning.DefaultMaxYBinning,
  sampling:   PosDouble    = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) = {
  val yBin = binning.spatialBinning(srcProfile, iq, pixelScale, maxBinning, sampling)
  (GmosXBinning(yBin.value), yBin)
}

/**
 * Optimal GMOS binning calculation for imaging.
 */
def northBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  detector:   GmosNorthDetector = binning.DefaultGmosNorthDetector,
  maxBinning: GmosYBinning      = binning.DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  imagingBinning(srcProfile, iq, detector.pixelSize, maxBinning, sampling)

/**
 * Optimal GMOS binning calculation for imaging.
 */
def southBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  detector:   GmosSouthDetector = binning.DefaultGmosSouthDetector,
  maxBinning: GmosYBinning      = binning.DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  imagingBinning(srcProfile, iq, detector.pixelSize, maxBinning,sampling)

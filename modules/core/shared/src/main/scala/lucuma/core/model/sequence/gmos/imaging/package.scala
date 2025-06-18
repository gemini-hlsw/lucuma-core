// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package imaging

import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile

/**
 * Optimal GMOS binning calculation for imaging.
 */
def northBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  detector:   GmosNorthDetector = GmosNorthDetector.Hamamatsu,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  binning.northImagingBinning(srcProfile, iq, detector, sampling)

/**
 * Optimal GMOS binning calculation for imaging.
 */
def southBinning(
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  detector:   GmosSouthDetector = GmosSouthDetector.Hamamatsu,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  binning.southImagingBinning(srcProfile, iq, detector, sampling)

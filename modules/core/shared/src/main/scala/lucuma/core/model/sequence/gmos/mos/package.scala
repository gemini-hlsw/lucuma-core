// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package mos

import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile

/**
 * Default maximum Y binning for MOS mode.
 * MOS observations typically use 2x2 binning maximum for spatial direction
 * to maintain adequate sampling for object identification and positioning.
 */
val DefaultMaxYBinning: GmosYBinning =
  GmosYBinning.Two

/**
 * Optimal GMOS binning calculation for MOS (Multi-Object Spectroscopy).
 */
def northBinning(
  fpu:        GmosNorthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosNorthGrating,
  detector:   GmosNorthDetector = GmosNorthDetector.Hamamatsu,
  maxYBin:    GmosYBinning      = DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  binning.northMosBinning(fpu, srcProfile, iq, grating, detector, maxYBin, sampling)

/**
 * Optimal GMOS binning calculation for MOS (Multi-Object Spectroscopy).
 */
def southBinning(
  fpu:        GmosSouthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosSouthGrating,
  detector:   GmosSouthDetector = GmosSouthDetector.Hamamatsu,
  maxYBin:    GmosYBinning      = DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  binning.southMosBinning(fpu, srcProfile, iq, grating, detector, maxYBin, sampling)

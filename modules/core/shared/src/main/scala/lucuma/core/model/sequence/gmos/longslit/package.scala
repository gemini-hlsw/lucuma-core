// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package longslit

import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.model.SourceProfile


val DefaultAmpReadMode: GmosAmpReadMode =
  GmosAmpReadMode.Slow

val DefaultAmpGain: GmosAmpGain =
  GmosAmpGain.Low

val DefaultRoi: GmosRoi =
  GmosRoi.FullFrame

val DefaultAmpCount: GmosAmpCount =
  GmosAmpCount.Twelve

/**
 * Optimal GMOS binning calculation for longslit.
  */
def northBinning(
  fpu:        GmosNorthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosNorthGrating,
  detector:   GmosNorthDetector = GmosNorthDetector.Hamamatsu,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  (binning.northSpectralBinning(fpu, srcProfile, iq, grating, sampling),
   binning.northSpatialBinning(srcProfile, iq, detector, sampling))

/**
 * Optimal GMOS binning calculation for longslit.
  */
def southBinning(
  fpu:        GmosSouthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosSouthGrating,
  detector:   GmosSouthDetector = GmosSouthDetector.Hamamatsu,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  (binning.southSpectralBinning(fpu, srcProfile, iq, grating, sampling),
   binning.southSpatialBinning(srcProfile, iq, detector, sampling))
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package longslit

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosSlitOffsetPreset
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.ImageQuality
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit

val DefaultAmpReadMode: GmosAmpReadMode =
  GmosAmpReadMode.Slow

val DefaultAmpGain: GmosAmpGain =
  GmosAmpGain.Low

val DefaultRoi: GmosRoi =
  GmosRoi.FullFrame

val DefaultAmpCount: GmosAmpCount =
  GmosAmpCount.Twelve

/**
 * Default nod pattern for a GMOS long slit
 * three guided positions along the slit.
 */
val DefaultSlitTelescopeConfigs: SlitTelescopeConfigs =
  SlitTelescopeConfigs.AlongSlit(
    NonEmptyList.of(
      TelescopeConfigAlongSlit(  0.qArcsec, StepGuideState.Enabled),
      TelescopeConfigAlongSlit( 15.qArcsec, StepGuideState.Enabled),
      TelescopeConfigAlongSlit(-15.qArcsec, StepGuideState.Enabled)
    )
  )

val OnSkyDefaultTelescopeConfigs: NonEmptyList[TelescopeConfig] =
  NonEmptyList.of(
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
    TelescopeConfig(Offset(30.pArcsec, 0.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset(30.pArcsec, 0.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled)
  )

/** The telescope configurations a preset stands for. */
def defaultSlitTelescopeConfigs(preset: GmosSlitOffsetPreset): SlitTelescopeConfigs =
  preset match
    case GmosSlitOffsetPreset.NodAlongSlit => DefaultSlitTelescopeConfigs
    case GmosSlitOffsetPreset.NodToSky     => SlitTelescopeConfigs.ToSky(OnSkyDefaultTelescopeConfigs)

/**
 * Optimal GMOS binning calculation for longslit.
  */
def northBinning(
  fpu:        GmosNorthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosNorthGrating,
  detector:   GmosNorthDetector = binning.DefaultGmosNorthDetector,
  maxBinning: GmosYBinning      = binning.DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  (binning.spectralBinning(
      fpu.effectiveSlitWidth,
      srcProfile,
      iq,
      grating.dispersion,
      grating.referenceResolution,
      grating.blazeWavelength,
      sampling
    ),
   binning.spatialBinning(srcProfile, iq, detector.pixelSize, maxBinning, sampling)
 )

/**
 * Optimal GMOS binning calculation for longslit.
  */
def southBinning(
  fpu:        GmosSouthFpu,
  srcProfile: SourceProfile,
  iq:         ImageQuality,
  grating:    GmosSouthGrating,
  detector:   GmosSouthDetector = binning.DefaultGmosSouthDetector,
  maxBinning: GmosYBinning      = binning.DefaultMaxYBinning,
  sampling:   PosDouble         = binning.DefaultSampling
): (GmosXBinning, GmosYBinning) =
  (binning.spectralBinning(
      fpu.effectiveSlitWidth,
      srcProfile,
      iq,
      grating.dispersion,
      grating.referenceResolution,
      grating.blazeWavelength,
      sampling
    ),
   binning.spatialBinning(srcProfile, iq, detector.pixelSize, maxBinning, sampling)
 )

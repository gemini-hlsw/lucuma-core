// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.data.NonEmptyList
import lucuma.core.enums.Igrins2FowlerSamples
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.syntax.timespan.*
import lucuma.core.util.NewBoolean
import lucuma.core.util.TimeSpan

object Igrins2SVCImages extends NewBoolean:
  val Save = True
  val DontSave = False

type Igrins2SVCImages = Igrins2SVCImages.Type

val SetupTime: TimeSpan = 7.minTimeSpan
val ReAcquisitionTime: TimeSpan = 5.minTimeSpan
val DefaultExposureTime: TimeSpan = 30.secTimeSpan
val WriteOutTime: TimeSpan = 8.secTimeSpan

val MinExposureTime: TimeSpan = 3080.msTimeSpan
val MaxExposureTime: TimeSpan = 600.secTimeSpan

val WavelengthCoverageLowerBound: Wavelength = Wavelength.fromIntNanometers(1490).get
val WavelengthCoverageUpperBound: Wavelength = Wavelength.fromIntNanometers(2460).get

// Updated from OCS values
val CentralWavelength: Wavelength = Wavelength.fromIntNanometers(2100).get

def fowlerSamplesForExposureTime(exposure: TimeSpan): Igrins2FowlerSamples =
  val seconds = exposure.toSeconds.toDouble
  val nFowler = ((seconds - 1.45479 - 0.168) / 1.45479).toInt
  Igrins2FowlerSamples.values.reverse
    .find(fs => nFowler >= (1 << fs.ordinal))
    .getOrElse(Igrins2FowlerSamples.One)

val NodAlongSlitDefaultTelescopeConfigs: NonEmptyList[TelescopeConfigAlongSlit] =
  NonEmptyList.of(
    TelescopeConfigAlongSlit(-1.25.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit( 1.25.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit( 1.25.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit(-1.25.qArcsec, StepGuideState.Enabled),
  )

val NodToSkyDefaultTelescopeConfigs: NonEmptyList[TelescopeConfig] =
  NonEmptyList.of(
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
    TelescopeConfig(Offset(10.pArcsec, 10.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
  )

def defaultSlitTelescopeConfigs(mode: SlitOffsetMode): SlitTelescopeConfigs =
  mode match
    case SlitOffsetMode.NodAlongSlit => SlitTelescopeConfigs.AlongSlit(NodAlongSlitDefaultTelescopeConfigs)
    case SlitOffsetMode.NodToSky     => SlitTelescopeConfigs.ToSky(NodToSkyDefaultTelescopeConfigs)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.data.NonEmptyList
import lucuma.core.enums.Flamingos2SlitOffsetPreset
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit

val TelluricDefaultTelescopeConfigs: NonEmptyList[TelescopeConfigAlongSlit] =
  NonEmptyList.of(
    TelescopeConfigAlongSlit( 15.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit(-15.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit(-15.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit( 15.qArcsec, StepGuideState.Enabled),
  )

val NodAlongSlitDefaultTelescopeConfigs: NonEmptyList[TelescopeConfigAlongSlit] =
  NonEmptyList.of(
    TelescopeConfigAlongSlit( 10.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit(-10.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit(-10.qArcsec, StepGuideState.Enabled),
    TelescopeConfigAlongSlit( 10.qArcsec, StepGuideState.Enabled),
  )

val NodToSkyDefaultTelescopeConfigs: NonEmptyList[TelescopeConfig] =
  NonEmptyList.of(
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
    TelescopeConfig(Offset(0.pArcsec, 300.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset(0.pArcsec, 310.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
  )

def defaultSlitTelescopeConfigs(preset: Flamingos2SlitOffsetPreset): SlitTelescopeConfigs =
  preset match
    case Flamingos2SlitOffsetPreset.Telluric     => SlitTelescopeConfigs.AlongSlit(TelluricDefaultTelescopeConfigs)
    case Flamingos2SlitOffsetPreset.NodAlongSlit => SlitTelescopeConfigs.AlongSlit(NodAlongSlitDefaultTelescopeConfigs)
    case Flamingos2SlitOffsetPreset.NodToSky     => SlitTelescopeConfigs.ToSky(NodToSkyDefaultTelescopeConfigs)

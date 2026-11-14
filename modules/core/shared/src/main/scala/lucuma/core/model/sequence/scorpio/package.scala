// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.data.NonEmptyList
import lucuma.core.enums.ScorpioSlitOffsetPreset
import lucuma.core.enums.StepGuideState
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfigAlongSlit
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * Shortest VIS exposure, set by the shutter's minimum window time.
 */
val VisMinExposureTime: TimeSpan = 116.msTimeSpan

// The 15 arcsec nod is the GMOS long slit default, not a SCORPIO measurement.
val NodAlongSlitDefaultTelescopeConfigs: NonEmptyList[TelescopeConfigAlongSlit] =
  NonEmptyList.of(
    TelescopeConfigAlongSlit(   0.qArcsec,  StepGuideState.Enabled),
    TelescopeConfigAlongSlit(-(15.qArcsec), StepGuideState.Enabled),
    TelescopeConfigAlongSlit(  15.qArcsec,  StepGuideState.Enabled),
    TelescopeConfigAlongSlit(   0.qArcsec,  StepGuideState.Enabled)
  )

def defaultSlitTelescopeConfigs(preset: ScorpioSlitOffsetPreset): SlitTelescopeConfigs =
  preset match
    case ScorpioSlitOffsetPreset.NodAlongSlit => SlitTelescopeConfigs.AlongSlit(NodAlongSlitDefaultTelescopeConfigs)

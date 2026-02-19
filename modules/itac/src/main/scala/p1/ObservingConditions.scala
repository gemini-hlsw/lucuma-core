// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality

case class ObservingConditions(
  imageQuality: ImageQuality.Preset,
  cloudExtinction: CloudExtinction.Preset,
  skyBackground: SkyBackground,
  waterVapor: WaterVapor,
  elevationRange: ElevationRange
)

object ObservingConditions {

  val AnyConditions =
    ObservingConditions(
      ImageQuality.Preset.TwoPointZero,
      CloudExtinction.Preset.ThreePointZero,
      SkyBackground.Bright,
      WaterVapor.Wet,
      ElevationRange.ByAirMass.Default
    )

}

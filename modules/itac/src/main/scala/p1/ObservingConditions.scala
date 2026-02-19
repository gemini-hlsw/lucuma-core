// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.enums.SkyBackground
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.enums.WaterVapor

case class ObservingConditions(
  cc: CloudExtinction.Preset,
  iq: ImageQuality.Preset,
  sb: SkyBackground,
  wv: WaterVapor,
)

object ObservingConditions {

  val AnyConditions =
    ObservingConditions(
      CloudExtinction.Preset.ThreePointZero,
      ImageQuality.Preset.TwoPointZero,
      SkyBackground.Bright,
      WaterVapor.Wet
    )

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class WaterVapor(val label: String) extends Product with Serializable

object WaterVapor {
  case object VeryDry extends WaterVapor("Very Dry")
  case object Dry     extends WaterVapor("Dry")
  case object Median  extends WaterVapor("Median")
  case object Wet     extends WaterVapor("Wet")

  implicit val WaterVaporEnumerated: Enumerated[WaterVapor] =
    Enumerated.of(VeryDry, Dry, Median, Wet)

  implicit val WatorVaporDisplay: Display[WaterVapor] =
    Display.byShortName(_.label)
}

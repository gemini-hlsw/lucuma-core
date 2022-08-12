// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class WaterVapor(val tag: String, val label: String) extends Product with Serializable

object WaterVapor {
  case object VeryDry extends WaterVapor("very_dry", "Very Dry")
  case object Dry     extends WaterVapor("dry", "Dry")
  case object Median  extends WaterVapor("median", "Median")
  case object Wet     extends WaterVapor("wet", "Wet")

  implicit val WaterVaporEnumerated: Enumerated[WaterVapor] =
    Enumerated.from(VeryDry, Dry, Median, Wet).withTag(_.tag)

  implicit val WatorVaporDisplay: Display[WaterVapor] =
    Display.byShortName(_.label)
}

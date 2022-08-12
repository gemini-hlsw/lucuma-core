// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class SkyBackground(val tag: String, val label: String) extends Product with Serializable

object SkyBackground {
  case object Darkest extends SkyBackground("darkest", "Darkest")
  case object Dark    extends SkyBackground("dark", "Dark")
  case object Gray    extends SkyBackground("gray", "Gray")
  case object Bright  extends SkyBackground("bright", "Bright")

  implicit val SkyBackgroundEnumerated: Enumerated[SkyBackground] =
    Enumerated.from(Darkest, Dark, Gray, Bright).withTag(_.tag)

  implicit val SkyBackgroundDisplay: Display[SkyBackground] =
    Display.byShortName(_.label)
}

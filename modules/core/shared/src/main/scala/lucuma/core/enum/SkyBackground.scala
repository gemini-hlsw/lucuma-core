// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.{ Display, Enumerated }

sealed abstract class SkyBackground(val label: String) extends Product with Serializable

object SkyBackground {
  case object Darkest extends SkyBackground("Darkest")
  case object Dark    extends SkyBackground("Dark")
  case object Gray    extends SkyBackground("Gray")
  case object Bright  extends SkyBackground("Bright")

  implicit val SkyBackgroundEnumerated: Enumerated[SkyBackground] =
    Enumerated.of(Darkest, Dark, Gray, Bright)

  implicit val SkyBackgroundDisplay: Display[SkyBackground] =
    Display.byShortName(_.label)
}

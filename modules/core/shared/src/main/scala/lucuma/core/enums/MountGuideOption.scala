// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.NewType

object MountGuideOption extends NewType[Boolean] {
  val MountGuideOn = MountGuideOption(true)
  val MountGuideOff = MountGuideOption(false)

  def fromBoolean(b: Boolean): MountGuideOption = if (b) MountGuideOn else MountGuideOff
}
type MountGuideOption = MountGuideOption.Type


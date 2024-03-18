// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.NewType

object ComaOption extends NewType[Boolean] {
  val ComaOn = ComaOption(true)
  val ComaOff = ComaOption(false)

  def fromBoolean(b: Boolean): ComaOption = if (b) ComaOn else ComaOff
}

type ComaOption = ComaOption.Type

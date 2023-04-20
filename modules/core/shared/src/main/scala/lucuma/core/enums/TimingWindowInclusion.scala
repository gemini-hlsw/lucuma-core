// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated
import lucuma.core.syntax.display.*
import lucuma.core.util.Display

enum TimingWindowInclusion derives Enumerated:
  case Include, Exclude

  def toBooleanInclude: Boolean = this == Include

  def tag: String = this match
    case Include => "include"
    case Exclude => "exclude"

object TimingWindowInclusion:
  def fromBooleanInclude(include: Boolean): TimingWindowInclusion =
    if (include) Include else Exclude

  given Display[TimingWindowInclusion] = Display.byShortName {
    case Include => "Include"
    case Exclude => "Exclude"
  }

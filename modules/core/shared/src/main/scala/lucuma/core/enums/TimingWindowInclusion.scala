// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import monocle.Iso

enum TimingWindowInclusion derives Enumerated:
  case Include, Exclude

  def tag: String = this match
    case Include => "include"
    case Exclude => "exclude"

object TimingWindowInclusion:
  val toBooleanInclude: Iso[TimingWindowInclusion, Boolean] =
    Iso[TimingWindowInclusion, Boolean](
      _ == TimingWindowInclusion.Include
    )(
      if (_) Include else Exclude
    )

  given Display[TimingWindowInclusion] = Display.byShortName {
    case Include => "Include"
    case Exclude => "Exclude"
  }

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum CloudExtinction(val tag: String, val toDeciBrightness: Int) derives Enumerated:
  def toBrightness: Double = toDeciBrightness / 10.0
  def label: String        = f"< $toBrightness%.1f mag"

  case PointOne       extends CloudExtinction("point_one", 1)
  case PointThree     extends CloudExtinction("point_three", 3)
  case PointFive      extends CloudExtinction("point_five", 5)
  case OnePointZero   extends CloudExtinction("one_point_zero", 10)
  case OnePointFive   extends CloudExtinction("one_point_five", 15)
  case TwoPointZero   extends CloudExtinction("two_point_zero", 20)
  case ThreePointZero extends CloudExtinction("three_point_zero", 30)

object CloudExtinction:
  given Display[CloudExtinction] =
    Display.byShortName(_.label)

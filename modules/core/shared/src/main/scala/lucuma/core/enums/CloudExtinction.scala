// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

sealed abstract class CloudExtinction(val tag: String, val toDeciBrightness: Int) extends Product with Serializable {
  def toBrightness: Double = toDeciBrightness / 10.0
  def label: String        = f"< $toBrightness%.1f mag"
}

object CloudExtinction {
  case object PointOne       extends CloudExtinction("point_one", 1)
  case object PointThree     extends CloudExtinction("point_three", 3)
  case object PointFive      extends CloudExtinction("point_five", 5)
  case object OnePointZero   extends CloudExtinction("one_point_zero", 10)
  case object OnePointFive   extends CloudExtinction("one_point_five", 15)
  case object TwoPointZero   extends CloudExtinction("two_point_zero", 20)
  case object ThreePointZero extends CloudExtinction("three_point_zero", 30)

  implicit val CloudExtinctionEnumerated: Enumerated[CloudExtinction] =
    Enumerated.from(
      PointOne,
      PointThree,
      PointFive,
      OnePointZero,
      OnePointFive,
      TwoPointZero,
      ThreePointZero
    ).withTag(_.tag)

  implicit val CloudExtinctionDisplay: Display[CloudExtinction] =
    Display.byShortName(_.label)
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.{ Display, Enumerated }

sealed abstract class CloudExtinction(val toDeciMagnitudes: Int) extends Product with Serializable {
  def toMagnitudes: Double = toDeciMagnitudes / 10.0
  def label: String        = f"< $toMagnitudes%.1f mag"
}

object CloudExtinction {
  case object PointOne       extends CloudExtinction(1)
  case object PointThree     extends CloudExtinction(3)
  case object PointFive      extends CloudExtinction(5)
  case object OnePointZero   extends CloudExtinction(10)
  case object OnePointFive   extends CloudExtinction(15)
  case object TwoPointZero   extends CloudExtinction(20)
  case object ThreePointZero extends CloudExtinction(30)

  implicit val CloudExtinctionEnumerated: Enumerated[CloudExtinction] =
    Enumerated.of(PointOne,
                  PointThree,
                  PointFive,
                  OnePointZero,
                  OnePointFive,
                  TwoPointZero,
                  ThreePointZero
    )

  implicit val CloudExtinctionDisplay: Display[CloudExtinction] =
    Display.byShortName(_.label)
}

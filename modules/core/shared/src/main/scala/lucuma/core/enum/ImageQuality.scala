// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.{ Display, Enumerated }

sealed abstract class ImageQuality(val toDeciArcSeconds: Int) extends Product with Serializable {
  def toArcSeconds: Double = toDeciArcSeconds / 10.0
  def label: String        = f"""< $toArcSeconds%.1f\""""
}

object ImageQuality {
  case object PointOne     extends ImageQuality(1)
  case object PointTwo     extends ImageQuality(2)
  case object PointThree   extends ImageQuality(3)
  case object PointFour    extends ImageQuality(4)
  case object PointSix     extends ImageQuality(6)
  case object PointEight   extends ImageQuality(8)
  case object OnePointZero extends ImageQuality(10)
  case object OnePointFive extends ImageQuality(15)
  case object TwoPointZero extends ImageQuality(20)

  implicit val ImageQualityEnumerated: Enumerated[ImageQuality] =
    Enumerated.of(PointOne,
                  PointTwo,
                  PointThree,
                  PointFour,
                  PointSix,
                  PointEight,
                  OnePointZero,
                  OnePointFive,
                  TwoPointZero
    )

  implicit val ImageQualityDisplay: Display[ImageQuality] =
    Display.byShortName(_.label)
}

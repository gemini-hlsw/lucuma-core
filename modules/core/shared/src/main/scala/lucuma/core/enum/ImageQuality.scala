// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import coulomb._
import coulomb.accepted.ArcSecond
import lucuma.core.math.units.DeciArcSecond
import lucuma.core.util.{ Display, Enumerated }
import spire.math.Rational

sealed abstract class ImageQuality(val toDeciArcSeconds: Quantity[Rational, DeciArcSecond]) extends Product with Serializable {
  def toArcSeconds: Quantity[Rational, ArcSecond] = toDeciArcSeconds.toUnit[ArcSecond]
  def label: String        = f"""< ${toArcSeconds.value.toDouble}%.1f\""""
}

object ImageQuality {
  case object PointOne     extends ImageQuality(1.withUnit[DeciArcSecond])
  case object PointTwo     extends ImageQuality(2.withUnit[DeciArcSecond])
  case object PointThree   extends ImageQuality(3.withUnit[DeciArcSecond])
  case object PointFour    extends ImageQuality(4.withUnit[DeciArcSecond])
  case object PointSix     extends ImageQuality(6.withUnit[DeciArcSecond])
  case object PointEight   extends ImageQuality(8.withUnit[DeciArcSecond])
  case object OnePointZero extends ImageQuality(10.withUnit[DeciArcSecond])
  case object OnePointFive extends ImageQuality(15.withUnit[DeciArcSecond])
  case object TwoPointZero extends ImageQuality(20.withUnit[DeciArcSecond])

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

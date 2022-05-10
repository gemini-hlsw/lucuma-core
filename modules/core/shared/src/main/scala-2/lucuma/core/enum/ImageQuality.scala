// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.`enum`

import coulomb._
import coulomb.accepted.ArcSecond
import coulomb.refined._
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.units.DeciArcSecond
import lucuma.core.math.units.MicroArcSecond
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import spire.math.Rational

sealed abstract class ImageQuality(val toDeciArcSeconds: Quantity[PosInt, DeciArcSecond]) extends Product with Serializable {
  def toArcSeconds: Quantity[Rational, ArcSecond] = toDeciArcSeconds.to[Rational, ArcSecond]
  def label: String        = f"""< ${toArcSeconds.value.toDouble}%.1f\""""

  def toAngle: Angle =
    Angle.fromMicroarcseconds(toDeciArcSeconds.to[PosInt, MicroArcSecond].value.value.toLong)
}

object ImageQuality {
  case object PointOne     extends ImageQuality(refineMV[Positive](1).withUnit[DeciArcSecond])
  case object PointTwo     extends ImageQuality(refineMV[Positive](2).withUnit[DeciArcSecond])
  case object PointThree   extends ImageQuality(refineMV[Positive](3).withUnit[DeciArcSecond])
  case object PointFour    extends ImageQuality(refineMV[Positive](4).withUnit[DeciArcSecond])
  case object PointSix     extends ImageQuality(refineMV[Positive](6).withUnit[DeciArcSecond])
  case object PointEight   extends ImageQuality(refineMV[Positive](8).withUnit[DeciArcSecond])
  case object OnePointZero extends ImageQuality(refineMV[Positive](10).withUnit[DeciArcSecond])
  case object OnePointFive extends ImageQuality(refineMV[Positive](15).withUnit[DeciArcSecond])
  case object TwoPointZero extends ImageQuality(refineMV[Positive](20).withUnit[DeciArcSecond])

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

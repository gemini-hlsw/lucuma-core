// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import coulomb.*
import coulomb.policy.spire.standard.{*, given}
import coulomb.syntax.*
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.units.{*, given}
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined._
import spire.math.Rational

sealed abstract class ImageQuality(val toDeciArcSeconds: Quantity[PosInt, DeciArcSecond]) extends Product with Serializable {
  def toArcSeconds: Quantity[Rational, ArcSecond] = toDeciArcSeconds.toValue[Rational].toUnit[ArcSecond]
  def label: String        = f"""< ${toArcSeconds.value.toDouble}%.1f\""""

  def toAngle: Angle =
    Angle.fromMicroarcseconds(toDeciArcSeconds.tToUnit[MicroArcSecond].value.value.toLong)
}

object ImageQuality {
  case object PointOne     extends ImageQuality(1.refined[Positive].withUnit[DeciArcSecond])
  case object PointTwo     extends ImageQuality(2.refined[Positive].withUnit[DeciArcSecond])
  case object PointThree   extends ImageQuality(3.refined[Positive].withUnit[DeciArcSecond])
  case object PointFour    extends ImageQuality(4.refined[Positive].withUnit[DeciArcSecond])
  case object PointSix     extends ImageQuality(6.refined[Positive].withUnit[DeciArcSecond])
  case object PointEight   extends ImageQuality(8.refined[Positive].withUnit[DeciArcSecond])
  case object OnePointZero extends ImageQuality(10.refined[Positive].withUnit[DeciArcSecond])
  case object OnePointFive extends ImageQuality(15.refined[Positive].withUnit[DeciArcSecond])
  case object TwoPointZero extends ImageQuality(20.refined[Positive].withUnit[DeciArcSecond])

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

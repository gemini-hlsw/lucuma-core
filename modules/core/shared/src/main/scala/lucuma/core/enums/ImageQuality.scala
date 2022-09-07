// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import coulomb.*
import coulomb.policy.spire.standard.{_, given}
import coulomb.syntax.*
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.units.{_, given}
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined._
import spire.math.Rational

sealed abstract class ImageQuality(val tag: String, val toDeciArcSeconds: Quantity[PosInt, DeciArcSecond]) extends Product with Serializable {
  def toArcSeconds: Quantity[Rational, ArcSecond] = toDeciArcSeconds.toValue[Rational].toUnit[ArcSecond]
  def label: String        = f"""< ${toArcSeconds.value.toDouble}%.1f\""""

  def toAngle: Angle =
    Angle.fromMicroarcseconds(toDeciArcSeconds.tToUnit[MicroArcSecond].value.value.toLong)
}

object ImageQuality {
  case object PointOne     extends ImageQuality("point_one", 1.withRefinedUnit[Positive, DeciArcSecond])
  case object PointTwo     extends ImageQuality("point_two", 2.withRefinedUnit[Positive, DeciArcSecond])
  case object PointThree   extends ImageQuality("point_three", 3.withRefinedUnit[Positive, DeciArcSecond])
  case object PointFour    extends ImageQuality("point_four", 4.withRefinedUnit[Positive, DeciArcSecond])
  case object PointSix     extends ImageQuality("point_six", 6.withRefinedUnit[Positive, DeciArcSecond])
  case object PointEight   extends ImageQuality("point_eight", 8.withRefinedUnit[Positive, DeciArcSecond])
  case object OnePointZero extends ImageQuality("one_point_zero", 10.withRefinedUnit[Positive, DeciArcSecond])
  case object OnePointFive extends ImageQuality("one_point_five", 15.withRefinedUnit[Positive, DeciArcSecond])
  case object TwoPointZero extends ImageQuality("two_point_zero", 20.withRefinedUnit[Positive, DeciArcSecond])

  implicit val ImageQualityEnumerated: Enumerated[ImageQuality] =
    Enumerated.from(
      PointOne,
      PointTwo,
      PointThree,
      PointFour,
      PointSix,
      PointEight,
      OnePointZero,
      OnePointFive,
      TwoPointZero
    ).withTag(_.tag)

  implicit val ImageQualityDisplay: Display[ImageQuality] =
    Display.byShortName(_.label)
}

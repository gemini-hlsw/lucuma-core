// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.contravariant.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import eu.timepit.refined.*
import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.Interval
import lucuma.core.math.Angle
import lucuma.core.math.units.*
import lucuma.core.math.units.given
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.NewType
import spire.math.Rational

type ImageQualityPredicate = Interval.OpenClosed[0, 500]
type ImageQualityValue  = Int Refined ImageQualityPredicate
val ImageQualityValue = new RefinedTypeOps[ImageQualityValue, Int]

type ImageQuality = ImageQuality.Type
object ImageQuality extends NewType[Quantity[ImageQualityValue, CentiArcSecond]]:
  def fromCentiArcSecond(value: Int): Either[String, ImageQuality] =
    ImageQualityValue.from(value).map(_.withUnit[CentiArcSecond]).map(ImageQuality(_))

  def unsafeFromCentiArcSecond(value: Int): ImageQuality           =
    ImageQuality(ImageQualityValue.unsafeFrom(value).withUnit[CentiArcSecond])

  extension (iq: ImageQuality)
    def toCentiArcSeconds: Int = iq.value.value.value

    def toArcSeconds: Quantity[Rational, ArcSecond] = iq.value.toValue[Int].toValue[Rational].toUnit[ArcSecond]

    def toAngle: Angle = Angle.fromMicroarcseconds(toCentiArcSeconds * 10_000L)

    def label: String = f"""< ${toArcSeconds.value.toDouble}%.2f"""

  given Display[ImageQuality] = Display.byShortName(_.label)

  enum Point(val tag: String, val toImageQuality: ImageQuality) derives Enumerated:
    case PointOne     extends Point("point_one", ImageQuality.unsafeFromCentiArcSecond(10))
    case PointTwo     extends Point("point_two", ImageQuality.unsafeFromCentiArcSecond(20))
    case PointThree   extends Point("point_three", ImageQuality.unsafeFromCentiArcSecond(30))
    case PointFour    extends Point("point_four", ImageQuality.unsafeFromCentiArcSecond(40))
    case PointSix     extends Point("point_six", ImageQuality.unsafeFromCentiArcSecond(60))
    case PointEight   extends Point("point_eight", ImageQuality.unsafeFromCentiArcSecond(80))
    case OnePointZero extends Point("one_point_zero", ImageQuality.unsafeFromCentiArcSecond(100))
    case OnePointFive extends Point("one_point_five", ImageQuality.unsafeFromCentiArcSecond(150))
    case TwoPointZero extends Point("two_point_zero", ImageQuality.unsafeFromCentiArcSecond(200))

  object Point:
    given Display[Point] = Display[ImageQuality].contramap(_.toImageQuality)

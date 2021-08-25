// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import coulomb._
import coulomb.accepted.ArcSecond
import eu.timepit.refined._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.cats._
import lucuma.core.math.units.DeciArcSecond
import lucuma.core.util.Display
import spire.math.Rational

final case class ImageQuality(val toDeciArcSeconds: Quantity[PosInt, DeciArcSecond]) extends Product with Serializable {
  def toArcSeconds: Quantity[Rational, ArcSecond] = toDeciArcSeconds.to[Rational, ArcSecond]
  def label: String        = f"""< ${toArcSeconds.value.toDouble}%.1f\""""
}

object ImageQuality {
  val PointOne: ImageQuality     = ImageQuality(refineMV[Positive](1).withUnit[DeciArcSecond])
  val PointTwo: ImageQuality     = ImageQuality(refineMV[Positive](2).withUnit[DeciArcSecond])
  val PointThree: ImageQuality   = ImageQuality(refineMV[Positive](3).withUnit[DeciArcSecond])
  val PointFour: ImageQuality    = ImageQuality(refineMV[Positive](4).withUnit[DeciArcSecond])
  val PointSix: ImageQuality     = ImageQuality(refineMV[Positive](6).withUnit[DeciArcSecond])
  val PointEight: ImageQuality   = ImageQuality(refineMV[Positive](8).withUnit[DeciArcSecond])
  val OnePointZero: ImageQuality = ImageQuality(refineMV[Positive](10).withUnit[DeciArcSecond])
  val OnePointFive: ImageQuality = ImageQuality(refineMV[Positive](15).withUnit[DeciArcSecond])
  val TwoPointZero: ImageQuality = ImageQuality(refineMV[Positive](20).withUnit[DeciArcSecond])

  val all: List[ImageQuality] = List(
    PointOne,
    PointTwo,
    PointThree,
    PointFour,
    PointSix,
    PointEight,
    OnePointZero,
    OnePointFive,
    TwoPointZero
  )

  /**
    * @group typeclass
    */
  implicit val ImageQualityOrder: Order[ImageQuality] =
    Order.by(_.toDeciArcSeconds.value)

  implicit val ImageQualityDisplay: Display[ImageQuality] =
    Display.byShortName(_.label)
}

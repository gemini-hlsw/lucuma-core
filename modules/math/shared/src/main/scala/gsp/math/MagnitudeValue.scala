// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.{ Order, Show }
import cats.instances.int._
import spire.math.Rational
import scala.math.rint

/**
  * Exact magnitude value represented as an int with the original value scaled up
  *
  * @param scaledValue This magnitude integral value, as the original multiplied by 100. value is dimensionless
  * @see The Wikipedia [[https://en.wikipedia.org/wiki/Apparent_magnitude]]
  */
final case class MagnitudeValue(private[gsp] val scaledValue: Int)
    extends Product
    with Serializable {
  def toRational: Rational  = Rational(scaledValue.toLong, 100)
  def toDoubleValue: Double = scaledValue / 100.0
}

object MagnitudeValue {

  final lazy val ZeroMagnitude = MagnitudeValue(0)

  /**
    * Construct a new MagnitudeValue of the given double value. Approximate.
    * @group Constructors
    */
  def fromDouble(mg: Double): MagnitudeValue =
    MagnitudeValue(rint(mg * 100).toInt)

  /** @group Typeclass Instances */
  implicit val MagnitudeValueShow: Show[MagnitudeValue] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val MagnitudeValueOrder: Order[MagnitudeValue] =
    Order.by(_.scaledValue)

}

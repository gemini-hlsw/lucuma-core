// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.{ Order, Show }
import spire.math.Rational
import scala.math.rint
import lucuma.core.optics.Format

/**
  * Exact magnitude value represented as an int with the original value scaled up
  *
  * @param scaledValue This magnitude integral value, as the original multiplied by 100. value is dimensionless
  * @see The Wikipedia [[https://en.wikipedia.org/wiki/Apparent_magnitude]]
  */
final case class MagnitudeValue(private[lucuma] val scaledValue: Int)
    extends Product
    with Serializable {
  def toRational: Rational = Rational(scaledValue.toLong, 100)

  def toDoubleValue: Double = scaledValue / 100.0

  override def toString: String =
    s"MagnitudeValue(${scaledValue / 100})"

}

object MagnitudeValue {

  final lazy val ZeroMagnitude = MagnitudeValue(0)

  /**
    * Construct a new MagnitudeValue of the given int value which be scaled up.
    * @group Constructors
    */
  def apply(mg: Int): MagnitudeValue =
    new MagnitudeValue(mg * 100)

  /**
    * Construct a new MagnitudeValue of the given double value. Approximate.
    * @group Constructors
    */
  def fromDouble(mg: Double): MagnitudeValue =
    new MagnitudeValue(rint(mg * 100).toInt)

  /**
    * Format with BigDecimal
    * @group Optics
    */
  val fromBigDecimal: Format[BigDecimal, MagnitudeValue] =
    Format[Int, MagnitudeValue](v => Some(new MagnitudeValue(v)), _.scaledValue)
      .imapA[BigDecimal](
        n => new java.math.BigDecimal(n).movePointLeft(2),
        d => d.underlying.movePointRight(2).intValue
      )

  /** @group Typeclass Instances */
  implicit val MagnitudeValueShow: Show[MagnitudeValue] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val MagnitudeValueOrder: Order[MagnitudeValue] =
    Order.by(_.scaledValue)

}

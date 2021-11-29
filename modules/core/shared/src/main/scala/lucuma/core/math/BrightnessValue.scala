// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import lucuma.core.optics.Format
import spire.math.Rational

import java.math.RoundingMode
import scala.math.rint
import scala.util.Try

/**
 * Exact brightness value represented as an int with the original value scaled up
 *
 * @param scaledValue
 *   A brightness integral value, as the original multiplied by 1000. value is unitless.
 * @see
 *   The Wikipedia [[https://en.wikipedia.org/wiki/Apparent_magnitude]]
 */
final case class BrightnessValue(private[lucuma] val scaledValue: Int)
    extends Product
    with Serializable {
  def toRational: Rational = Rational(scaledValue.toLong, 1000)

  def toDoubleValue: Double = scaledValue / 1000.0

  override def toString: String =
    s"BrightnessValue.fromDouble(${BigDecimal(scaledValue).underlying.movePointLeft(3).toString})"

}

object BrightnessValue {

  final lazy val ZeroMagnitude = BrightnessValue(0)

  /**
   * Construct a new BrightnessValue of the given int value which be scaled up.
   * @group Constructors
   */
  def apply(mg: Int): BrightnessValue =
    new BrightnessValue(mg * 1000)

  /**
   * Construct a new BrightnessValue of the given double value. Approximate.
   * @group Constructors
   */
  def fromDouble(mg: Double): BrightnessValue =
    new BrightnessValue(rint(mg * 1000).toInt)

  /**
   * Format with BigDecimal
   * @group Optics
   */
  val fromBigDecimal: Format[BigDecimal, BrightnessValue] =
    Format[Int, BrightnessValue](v => Some(new BrightnessValue(v)), _.scaledValue)
      .imapA[BigDecimal](
        n => new java.math.BigDecimal(n).movePointLeft(3),
        d => d.underlying.movePointRight(3).setScale(0, RoundingMode.HALF_UP).intValue
      )

  /**
   * @group Optics
   */
  val fromString: Format[String, BrightnessValue] =
    Format[String, BigDecimal](s => Try(BigDecimal(s)).toOption, _.toString)
      .andThen(fromBigDecimal)

  /** @group Typeclass Instances */
  implicit val BrightnessValueShow: Show[BrightnessValue] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val BrightnessValueOrder: Order[BrightnessValue] =
    Order.by(_.scaledValue)

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import algebra.ring.AdditiveCommutativeGroup
import cats.Order
import cats.Show
import lucuma.core.optics.Format
import lucuma.core.optics.SplitEpi
import lucuma.core.util.Display
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

  def toDouble: Double = scaledValue / 1000.0

  def toBigDecimal: BigDecimal = BigDecimal(scaledValue).underlying.movePointLeft(3)

  override def toString: String =
    s"BrightnessValue.fromDouble(${toBigDecimal.toString})"

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
   * SplitEpi with BigDecimal
   * @group Optics
   */
  val fromBigDecimal: SplitEpi[BigDecimal, BrightnessValue] =
    SplitEpi[BigDecimal, BrightnessValue](
      d =>
        new BrightnessValue(
          d.underlying.movePointRight(3).setScale(0, RoundingMode.HALF_UP).intValue
        ),
      b => new java.math.BigDecimal(b.scaledValue).movePointLeft(3)
    )

  /**
   * @group Optics
   */
  val fromString: Format[String, BrightnessValue] =
    Format[String, BigDecimal](s => Try(BigDecimal(s)).toOption, _.toString)
      .andThen(fromBigDecimal.asFormat)

  /** @group Typeclass Instances */
  implicit val BrightnessValueShow: Show[BrightnessValue] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val BrightnessValueOrder: Order[BrightnessValue] =
    Order.by(_.scaledValue)

  /** @group Typeclass Instances */
  implicit val BrightnessValueDisplay: Display[BrightnessValue] =
    Display.byShortName(_.toBigDecimal.toString)

  val Zero: BrightnessValue = new BrightnessValue(0)

  /**
   * @group Typeclass Instances
   * Support addition, multiplication and scalar operations
   */
  implicit val algebraBrightnessValue: AdditiveCommutativeGroup[BrightnessValue] =
    new AdditiveCommutativeGroup[BrightnessValue] {
      override def negate(a: BrightnessValue): BrightnessValue =
        new BrightnessValue(-a.scaledValue)

      override val zero: BrightnessValue =
        Zero

      override def plus(x: BrightnessValue, y: BrightnessValue): BrightnessValue =
        new BrightnessValue(x.scaledValue + y.scaledValue)

    }

}

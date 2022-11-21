// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import lucuma.core.optics.Format
import monocle._

/**
 * Celestial longitude, measured eastward along the celestial equator from the vernal equinox to the
 * hour circle of the point in question. This class is a simple newtype wrapper for an [[HourAngle]].
 * @see The helpful [[https://en.wikipedia.org/wiki/Right_ascension Wikipedia]] article.
 * @param toHourAngle the underlying hour angle
 */
final case class RightAscension(toHourAngle: HourAngle) {

  /**
   * Offset this `RightAscension` by the given hour angle.
   * @group Operations
   */
  def offset(ha: HourAngle): RightAscension =
    RightAscension(toHourAngle + ha)

  /**
   * Flip this `RightAscension` 180°, as might be required when offsetting coordinates causes the
   * associated declination to cross a pole.
   * @group Operations
   */
  def flip: RightAscension =
    RightAscension(toHourAngle.flip)

  /** Forget that this [[RightAscension]] wraps an [[HourAngle]]. */
  def toAngle: Angle =
    toHourAngle

  /** This RightAscension in radians [0 .. 2π). Approximate. */
  def toRadians: Double =
    toAngle.toDoubleRadians

  override def toString: String =
    RightAscension.fromStringHMS.taggedToString("RA", this)

}

object RightAscension extends RightAscensionOptics {

  /**
   * Construct a `RightAscension` from an angle in radians.
   * @group Constructors
   */
  def fromRadians(rad: Double): RightAscension =
    RightAscension(Angle.hourAngle.get(Angle.fromDoubleRadians(rad)))

  /**
   * Construct a `RightAscension` from an angle in degrees. Approximate
   * @group Constructors
   */
  def fromDoubleDegrees(deg: Double): RightAscension =
    fromHourAngle.get(HourAngle.fromDoubleDegrees(deg))

  /**
   * The `RightAscension` at zero degrees.
   * @group Constructors
   */
  val Zero: RightAscension =
    RightAscension(HourAngle.HourAngle0)

  /**
   * Unlike arbitrary angles, it is common to order right asensions starting at zero hours.
   * @group Typeclass Instances
   */
  implicit val RightAscensionOrder: Order[RightAscension] =
    Order.by(_.toHourAngle.toMicroseconds)

  /* @group Typeclass Instances */
  implicit val RightAscensionShow: Show[RightAscension] =
    Show.fromToString

}

trait RightAscensionOptics { this: RightAscension.type =>

  val fromHourAngle: Iso[HourAngle, RightAscension] =
    Iso(RightAscension(_))(_.toHourAngle)

  val fromStringHMS: Format[String, RightAscension] =
    HourAngle.fromStringHMS.andThen(fromHourAngle)

  val fromAngleExact: Prism[Angle, RightAscension] =
    Angle.hourAngleExact.andThen(fromHourAngle)

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util

import scala.math

import Angle.*

object Angle {
  sealed class Unit(val circle: Double, val abbr: String) extends Serializable {

    def convert(theta: Double, u: Unit): Double = {
      if (u == this) theta
      else theta/circle * u.circle
    }

    def toMas(theta: Double): Double    = convert(theta, Mas)
    def toArcsec(theta: Double): Double = convert(theta, Arcsec)
    def toArcmin(theta: Double): Double = convert(theta, Arcmin)
    def toDeg(theta: Double): Double    = convert(theta, Deg)
    def toSec(theta: Double): Double    = convert(theta, Sec)
    def toMin(theta: Double): Double    = convert(theta, Min)
    def toHour(theta: Double): Double   = convert(theta, Hr)
    def toRad(theta: Double): Double    = convert(theta, Rad)

    override def toString = abbr
  }

  object Mas extends Unit(360*60*60*1000.0, "mas")
  object Arcsec extends Unit(360*60*60.0, "arcsec")
  object Arcmin extends Unit(360*60.0, "arcmin")
  object Deg extends Unit(360.0, "deg")
  object Sec extends Unit(24*60*60.0, "sec")
  object Min extends Unit(24*60.0, "min")
  object Hr extends Unit(24.0, "hr")
  object Rad extends Unit(2 * math.Pi, "rad")

  val allUnits = List(Mas, Arcsec, Arcmin, Deg, Sec, Min, Hr, Rad)

  val angleDeg0     = new Angle(0, Deg)
  val angle2Pi      = new Angle(0, Rad)
  val anglePiOver2  = new Angle(math.Pi/2, Rad)
  val anglePi       = new Angle(math.Pi, Rad)
  val angle3PiOver2 = new Angle(3*math.Pi/2, Rad)
}

/**
 * An angle and its measurement unit.  Construct The
 * magnitude is normalized so that it falls within the range of
 * <code>min < magnitude < max</code> where min and max depend upon the
 * units.  For example, if the units are degrees, min is -360 and max is
 * 360.  So
 *
 * <pre>
 *    (new Angle(370, Deg)).mag == 10.0
 * </pre>
 *
 * and
 *
 * <pre>
 *    (new Angle(360, Deg)).mag == 0.0
 * </pre>
 *
 * while
 *
 * <pre>
 *    (new Angle(-370, Deg)).mag == -10.0
 * </pre>
 *
 * @param magnitude magnitude of the angle relative to the provided units;
 * may not be infinite or NaN
 * @param unit units by which to interpret the angle; may not be
 * <code>null</code>
 */
final class Angle(private val theta: Double, val unit: Unit) extends Ordered[Angle] with Serializable {
  require(!theta.isInfinite && !theta.isNaN)

  val mag = if (math.abs(theta) < unit.circle) theta
            else math.IEEEremainder(theta, unit.circle)

  /**
   * Returns an equivalent angle but converted to the given
   * units.  If the <code>unit</code> argument is the same as
   * <code>this</code> object's units, then <code>this</code> is returned.
   *
   * @param unit units to which the angle should be converted
   *
   * @return a new Angle that is equivalent to this angle, but expressed
   * in the given units; <code>this</code> if <code>unit</code> is the
   * same as <code>this</code> angle's units
   */
  def convertTo(u: Unit): Angle =
    if (this.unit == u) this
    else new Angle(this.unit.convert(mag, u), u)

  def toMas     = convertTo(Mas)
  def toArcsec  = convertTo(Arcsec)
  def toArcmin  = convertTo(Arcmin)
  def toDeg     = convertTo(Deg)
  def toSec     = convertTo(Sec)
  def toMin     = convertTo(Min)
  def toHr      = convertTo(Hr)
  def toRad     = convertTo(Rad)


  /**
   * Returns an equivalent angle, but converted to a positive magnitude.
   * If <code>this</code> angle is already expressed in a positive
   * magnitude, then <code>this</code> is returned.
   *
   * <p>For example,
   * <code>(new Angle(-10, Deg)).toPositive.mag == 350.0</code>
   *
   * <p>whereas given <code>val a10 = new Angle(10, Deg);</code>,
   * then <code>a10.toPositive eq a10</code>.
   *
   * @return a new Angle that is equivalent to this angle, but expressed
   * as a positive angle; <code>this</code> if already expressed as a
   * positive angle
   */
  def toPositive: Angle = {
    if (mag >=0) this else new Angle(unit.circle + mag, unit)
  }

  /**
   * Returns an equivalent angle, but converted to a negative magnitude.
   * If <code>this</code> angle is already expressed as a negative
   * magnitude, then <code>this</code> is returned.
   *
   * <p>For example,
   * <code>(new Angle(10, Deg)).toNegative.mag == -350.0</code>
   *
   * <p>whereas given <code>val minus10 = new Angle(-10, Deg);</code>,
   * then <code>minus10.toNegative eq minus10</code>.
   *
   * @return a new Angle that is equivalent to this angle, but expressed
   * as a negative angle; <code>this</code> if already expressed as a
   * negative angle
   */
  def toNegative: Angle = {
      if (mag <= 0) this else new Angle(mag - unit.circle, unit)
  }

  private def radians: Double = unit.convert(mag, Rad)

  /**
   * Computes the trigometric sine of the angle.
   */
  def sin: Double = math.sin(radians)

  /**
   * Computes the trigometric cosine of the angle.
   */
  def cos: Double = math.cos(radians)

  /**
   * Computes the trigometric tangent of the angle.
   */
  def tan: Double = math.tan(radians)

  /**
   * Computes the arc sine of the angle.
   */
  def asin: Double = math.asin(radians)

  /**
   * Computes the arc cosine of the angle.
   */
  def acos: Double = math.acos(radians)

  /**
   * Computes the arc tangent of the angle.
   */
  def atan: Double = math.atan(radians)

  // Adjust the angle "that" to be expressed in the same units as this
  // angle, and as a positive angle if this angle is positive (or negative
  // if this angle is negative).  This makes comparing the two angles or
  // adding them together possible.
  private def adjust(that: Angle): Angle = {
    val tmp = that.convertTo(unit)
    if (theta < 0) tmp.toNegative else tmp.toPositive
  }

  /**
   * Adds the given angle to this angle and returns a new angle that
   * contains the result expressed in this angle's units.
   *
   * @param that angle to add to this angle
   *
   * @return new angle that represents the sum of <code>this</code> angle and
   * <code>that</code> angle expressed in <code>this</code> angle's units
   */
  def +(that: Angle): Angle = new Angle(mag + adjust(that).mag, unit)

  /**
   * Adds the given amount to this angle and returns a new angle that
   * contains the result expressed in this angle's units.  This method is
   * equivalent to <code>+(that: Angle)</code>.
   *
   * @param theta angle to add to this angle, expressed in terms of the given
   * units
   * @param units in which to interpret the angle <code>theta</code>
   *
   * @return new angle that represents the sum of <code>this</code> angle and
   * the given angle <code>theta</code> expressed in <code>this</code> angle's
   * units
   */
  def add(theta: Double): Angle = this + new Angle(theta, unit)

  /**
   * Compares two angles taking into account the units in which they are
   * expressed.  For example, 90 degrees is less than PI radians but more
   * than 100 arcsec.  Note, this is not an implementation of the Java
   * <code>Comparable</code> interface because it is not consistent with
   * <code>equals</code>.  Two angles that are equivalent, but expressed
   * in different units would not be considered <code>equals</code> but
   * would be compared as equivalent by this method.
   *
   * @param that the angle to compare to <code>this</code> angle
   *
   * @return return -1 if <code>this</code> angle is less than
   * <code>that</code> angle, 0 if they represent the same angle, and 1 if
   * <code>that</code> is bigger than <code>this</code>
   */
  def compare(that: Angle): Int = mag.compare(adjust(that).mag)

  override def toString: String = "%f %s".format(mag, unit.toString)

  override def equals(other: Any) = other match {
    case that: Angle => toDeg.toPositive.mag == that.toDeg.toPositive.mag
    case _ => false
  }
  override def hashCode: Int = toDeg.toPositive.mag.hashCode

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.*
import cats.syntax.all.*
import lucuma.core.math.parser.CoordinateParsers
import lucuma.core.optics.Format
import lucuma.core.syntax.all.*
import monocle.Focus
import monocle.Lens

import java.lang.Math.*

/** A point in the sky, given right ascension and declination. */
final case class Coordinates(ra: RightAscension, dec: Declination) {
  /**
   * Shift these `Coordinates` by the given deltas, and indicate whether the declination crossed
   * a pole; if so the right ascension will have been flipped 180°.
   * @group Operations
   */
  def shiftWithCarry(dRA: HourAngle, dDec: Angle): (Coordinates, Boolean) =
    dec.offset(dDec) match {
      case (decʹ, false) => (Coordinates(ra.offset(dRA), decʹ), false)
      case (decʹ, true)  => (Coordinates(ra.flip.offset(dRA), decʹ), true)
    }

  /**
   * Shift these `Coordinates` by the given deltas. If the declination crossed a pole the right
   * ascension will have been flipped 180°.
   * @group Operations
   */
  def shift(dRA: HourAngle, dDec: Angle): Coordinates =
    shiftWithCarry(dRA, dDec)._1

  /**
   * Calculates the offset, posAngle and distance to another coordinate
   * Taken from the OT calculations.
   * (Based on the C version from A. P. Martinez)
   * @group Operations
   */
  def diff(x: Coordinates): CoordinatesDiff = {
    val alf           = x.ra.toRadians
    val alf0          = ra.toRadians
    val limitDistance = 0.0000004

    val sd0           = dec.toAngle.sin
    val sd            = x.dec.toAngle.sin
    val cd0           = dec.toAngle.cos
    val cd            = x.dec.toAngle.cos
    val cosda         = (x.ra.toAngle - ra.toAngle).cos
    val cosd          = sd0 * sd + cd0 * cd * cosda

    val dist = {
      val acosValue = acos(cosd)
      if (acosValue.isNaN) 0.0 else acosValue
    }

    val phi = if (dist > limitDistance) {
      val sind   = sin(dist)
      val pcospa = (sd * cd0 - cd * sd0 * cosda) / sind
      val cospa  = {
        val absValue = abs(pcospa)
        if (absValue > 1.0) pcospa / absValue else pcospa
      }
      val sinpa  = cd * sin(alf - alf0) / sind
      val pphi   = acos(cospa)

      if (sinpa < 0.0) (PI * 2) - pphi else pphi
    } else {
      0
    }

    CoordinatesDiff(Angle.fromDoubleDegrees(phi * 180.0 / PI), Angle.fromDoubleDegrees(dist * 180.0 / PI))
  }

  /**
   * Angular distance from `this` to `that`, always a positive angle in [0, 180]). Approximate.
   * @see Algorithm at [[http://www.movable-type.co.uk/scripts/latlong.html Movable Type Scripts]].
   */
  def angularDistance(that: Coordinates): Angle = {
    val φ1 = this.dec.toAngle.toDoubleRadians
    val φ2 = that.dec.toAngle.toDoubleRadians
    val Δφ = (that.dec.toAngle - this.dec.toAngle).toDoubleRadians
    val Δλ = (that.ra.toAngle - this.ra.toAngle).toDoubleRadians
    val a  = sin(Δφ / 2) * sin(Δφ / 2) +
      cos(φ1) * cos(φ2) *
        sin(Δλ / 2) * sin(Δλ / 2)
    Angle.fromDoubleRadians(2 * atan2(sqrt(a), sqrt(1 - a)))
  }

  /**
   * Interpolate between `this` and `that` at a position specified by `f`, where `0.0` is `this`,
   * `1.0` is `other`, and `0.5` is halfway along the great circle connecting them. Note that this
   * computation is undefined where `f` is `NaN` or `Infinity`. Approximate.
   * @see Algorithm at [[http://www.movable-type.co.uk/scripts/latlong.html Movable Type Scripts]].
   */
  def interpolate(that: Coordinates, f: Double): Coordinates = {
    val δ = angularDistance(that).toDoubleRadians
    if (δ === 0) this
    else {
      val φ1 = this.dec.toAngle.toDoubleRadians
      val φ2 = that.dec.toAngle.toDoubleRadians
      val λ1 = this.ra.toAngle.toDoubleRadians
      val λ2 = that.ra.toAngle.toDoubleRadians
      val a  = sin((1 - f) * δ) / sin(δ) // n.b. this line is wrong on the web page
      val b  = sin(f * δ) / sin(δ)
      val x  = a * cos(φ1) * cos(λ1) + b * cos(φ2) * cos(λ2)
      val y  = a * cos(φ1) * sin(λ1) + b * cos(φ2) * sin(λ2)
      val z  = a * sin(φ1) + b * sin(φ2)
      val φi = atan2(z, sqrt(x * x + y * y))
      val λi = atan2(y, x)
      Coordinates(
        RA.fromHourAngle.get(Angle.hourAngle.get(Angle.fromDoubleRadians(λi))),
        Dec.fromAngle.unsafeGet(Angle.fromDoubleRadians(φi))
      )
    }
  }

  /**
   * Offset the coordinates on the posAngl axis a given offset. Approximate
   * This operation is undefined at Dec +/-90
   * @return the coordinates offseted on the posAngle axis.
   */
  def offsetBy(posAngle: Angle, offset: Offset): Option[Coordinates] =
    if (offset =!= Offset.Zero) {
      val paCos  = posAngle.cos
      val paSin  = posAngle.sin
      val pDeg   = offset.p.toAngle.toSignedDoubleDegrees
      val qDeg   = offset.q.toAngle.toSignedDoubleDegrees
      val dRa    = pDeg * paCos + qDeg * paSin
      val dDec   = -pDeg * paSin + qDeg * paCos
      val decCos = dec.toAngle.cos

      Declination
        .fromDoubleDegrees(dec.toAngle.toSignedDoubleDegrees + dDec)
        .filter(_ => decCos != 0)
        .map { dec =>
          val newRa = RightAscension.fromDoubleDegrees(ra.toAngle.toDoubleDegrees + dRa / decCos)
          Coordinates(newRa, dec)
        }
    } else this.some // Return itself on no offset

  /** These coordinates in radians, [0 .. 2π) and [-π/2 .. π/2]. */
  def toRadians: (Double, Double) =
    (ra.toRadians, dec.toRadians)

  override def toString: String =
    Coordinates.fromHmsDms.productToString(this)

}

object Coordinates extends CoordinatesOptics {

  /* @group Constructors */
  val Zero: Coordinates      = Coordinates(RA.Zero, Dec.Zero)
  /* @group Constructors */
  val SouthPole: Coordinates = Coordinates(RA.Zero, Dec.Min)
  /* @group Constructors */
  val NorthPole: Coordinates = Coordinates(RA.Zero, Dec.Max)

  def fromRadians(ra: Double, dec: Double): Option[Coordinates] =
    Declination.fromRadians(dec).map(Coordinates(RA.fromRadians(ra), _))

  def unsafeFromRadians(ra: Double, dec: Double): Coordinates =
    Coordinates(RA.fromRadians(ra), Declination.unsafeFromRadians(dec))

  /**
  * Taken from
  * https://www.geomidpoint.com/calculation.html
  * and
  * https://stackoverflow.com/questions/6671183/calculate-the-center-point-of-multiple-latitude-longitude-coordinate-pairs
  *
  * Convert dec/ra (must be in radians) to Cartesian coordinates for each location.
  * X = cos(dec) * cos(ra)
  * Y = cos(dec) * sin(ra)
  * Z = sin(dec)
  *
  * Compute average x, y and z coordinates.
  * x = (x1 + x2 + ... + xn) / n
  * y = (y1 + y2 + ... + yn) / n
  * z = (z1 + z2 + ... + zn) / n
  *
  * Convert average x, y, z coordinate to latitude and longitude.
  * Lon = atan2(y, x)
  * Hyp = sqrt(x * x + y * y)
  * Lat = atan2(z, hyp)
  */
  def centerOf[F[_]: Foldable](coords: F[Coordinates]): Coordinates =
    val (x0, y0, z0) = coords.foldMap { case Coordinates(ra, dec) =>
      (cos(dec.toRadians) * cos(ra.toRadians),
      cos(dec.toRadians) * sin(ra.toRadians),
      sin(dec.toRadians))
    }
    val count        = coords.size
    val (x, y, z)    = (x0 / count, y0 / count, z0 / count)

    val ra  = atan2(y, x)
    val hyp = hypot(x, y)
    val dec = atan2(z, hyp)
    Coordinates(RightAscension.fromRadians(ra), Declination.unsafeFromRadians(dec))

  /** @group Typeclass Instances */
  given Order[Coordinates] =
    Order.by(c => (c.ra, c.dec))

  /** @group Typeclass Instances. */
  given Show[Coordinates] =
    Show.fromToString

}

trait CoordinatesOptics { this: Coordinates.type =>

  /**
   * Format as a String like "17 57 48.49803 +04 41 36.2072".
   * @group Optics
   */
  val fromHmsDms: Format[String, Coordinates] = Format(
    CoordinateParsers.coordinates.parseAll(_).toOption,
    cs =>
      s"${RightAscension.fromStringHMS.reverseGet(cs.ra)} ${Declination.fromStringSignedDMS
        .reverseGet(cs.dec)}"
  )

  /** @group Optics */
  val rightAscension: Lens[Coordinates, RightAscension] =
    Focus[Coordinates](_.ra)

  /** @group Optics */
  val declination: Lens[Coordinates, Declination] =
    Focus[Coordinates](_.dec)

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.Foldable
import cats.Monoid
import cats.data.NonEmptyList
import cats.derived.*
import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import lucuma.core.enums.TrackType
import lucuma.core.math.*
import lucuma.core.optics.SplitMono
import lucuma.core.syntax.treemap.*
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens

import java.lang.Math.atan2
import java.lang.Math.cos
import java.lang.Math.hypot
import java.lang.Math.sin
import scala.collection.immutable.TreeMap

/**
 * Generic representation to track an object.
 */
sealed trait Tracking:
  def at: TrackingAt

  def isNonsidereal: Boolean = this match
    case EphemerisTracking(_)   => true
    case CompositeTracking(nel) => nel.exists(_.isNonsidereal)
    case _                      => false

  def trackType: TrackType =
    if (isNonsidereal) TrackType.Nonsidereal else TrackType.Sidereal

object Tracking:
  given Eq[Tracking] = Eq.instance:
    case (a: ConstantTracking, b: ConstantTracking)   => a === b
    case (a: CompositeTracking, b: CompositeTracking) => a === b
    case (a: EphemerisTracking, b: EphemerisTracking) => a === b
    case (a: SiderealTracking, b: SiderealTracking)   => a === b
    case _                                            => false

  def fromList(trackings: List[Tracking]): Option[Tracking] =
    NonEmptyList.fromList(trackings).map(fromNel)

  def fromNel(trackings: NonEmptyList[Tracking]): Tracking = trackings match
    case NonEmptyList(h, Nil) => h
    case _ => CompositeTracking(trackings)


  def constant(coordinates: Coordinates): Tracking =
    ConstantTracking(coordinates)

case class ConstantTracking(value: Coordinates) extends Tracking derives Eq:
  val at: TrackingAt = _ =>
    Some(value)

case class CompositeTracking(toNonEmptyList: NonEmptyList[Tracking]) extends Tracking derives Eq:
  val at: TrackingAt = i =>
    toNonEmptyList
      .traverse(_.at(i))
      .map(cs => Coordinates.centerOf(cs))

/**
 * Time-parameterized coordinates over a fixed interval, defined pairwise. Coordinates that fall
 * between known instants are interpolated.
 */
case class EphemerisTracking private (toMap: TreeMap[Timestamp, EphemerisCoordinates]) extends Tracking {
  import EphemerisTracking.Element

  // N.B. this case class is abstract and has a private ctor because we want to keep construction of
  // the TreeMap private to ensure that the correct ordering is used and remains consistent.

  /** First element in this ephemeris, if any. */
  def first: Option[Element] =
    toMap.headOption

  /** Last element in this ephemeris, if any. */
  def last: Option[Element] =
    toMap.lastOption

  /** Coordinates at time `t`, exact if known, interpolated if `bracket(t)` is known. */
  def get(t: Timestamp): Option[EphemerisCoordinates] =
    toMap
      .get(t)
      .orElse(bracket(t).map {
        case ((a, ca), (b, cb)) =>
          val (iʹ, aʹ, bʹ) = (t.toEpochMilli, a.toEpochMilli, b.toEpochMilli)
          val factor       = (iʹ - aʹ).toDouble / (bʹ - aʹ).toDouble
          ca.interpolate(cb, factor)
      })

  val at: TrackingAt = i =>
    Timestamp.fromInstant(i).flatMap(get).map(_.coord)

  /**
   * Greatest lower and least upper bounds of `t`; i.e., the closest elements on either side,
   * inclusive (so if `t` is present then `bracket(t) = (t, t)`).
   */
  def bracket(t: Timestamp): Option[(Element, Element)] =
    (toMap.rangeTo(t).lastOption, toMap.rangeFrom(t).headOption).tupled

  /** The sum of this ephemeris and `e`, taking values from `e` in the case of overlap. */
  def ++(e: EphemerisTracking): EphemerisTracking =
    new EphemerisTracking(toMap ++ e.toMap) {}
}

object EphemerisTracking {

  /** An ephemeris element. */
  type Element = (Timestamp, EphemerisCoordinates)

  /** The empty ephemeris. */
  val empty: EphemerisTracking = apply()

  /** Construct an ephemeris from a sequence of literal elements. */
  def apply(es: Element*): EphemerisTracking =
    fromList(es.toList)

  /** Construct an ephemeris from a `List` of elements. */
  def fromList(es:                     List[Element]): EphemerisTracking =
    EphemerisTracking(TreeMap.fromList(es))

  /** Construct an ephemeris from a foldable of elements. */
  def fromFoldable[F[_]: Foldable](fa: F[Element]): EphemerisTracking    =
    fromList(fa.toList)

  /** Ephemerides form a monoid, using `++` as the combining operation. */
  given Monoid[EphemerisTracking] =
    new Monoid[EphemerisTracking] {
      val empty: EphemerisTracking = EphemerisTracking.empty
      def combine(a: EphemerisTracking, b: EphemerisTracking) = a ++ b
    }

  given Eq[EphemerisTracking] =
    Eq.fromUniversalEquals

  val elements: SplitMono[EphemerisTracking, List[Element]] =
    SplitMono(_.toMap.toList, fromFoldable(_))
}

/**
 * Time-parameterized coordinates, based on an observed position at some point in time (called the
 * `epoch`) and measured velocities in distance (`radialVelocity`; i.e., doppler shift) and position
 * (`properMotion`) per year. Given this information we can compute the position at any instant in
 * time. The references below are ''extremely'' helpful, so do check them out if you're trying to
 * understand the implementation.
 * @see
 *   The pretty good [[https://en.wikipedia.org/wiki/Proper_motion wikipedia]] article
 * @see
 *   Astronomical Almanac 1984
 *   [[https://babel.hathitrust.org/cgi/pt?id=uc1.b3754036;view=1up;seq=141 p.B39]]
 * @see
 *   Astronomy and Astrophysics 134 (1984)
 *   [[http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?bibcode=1984A%26A...134....1L&db_key=AST&page_ind=0&data_type=GIF&type=SCREEN_VIEW&classic=YES p.1-6]]
 * @param baseCoordinates
 *   observed coordinates at `epoch`
 * @param epoch
 *   time of the base observation; typically `Epoch.J2000`
 * @param properMotion
 *   proper velocity '''per year''' in [[lucuma.core.math.RightAscension]] and
 *   [[lucuma.core.math.Declination]], if any
 * @param radialVelocity
 *   radial velocity (km/y, positive if receding), if any
 * @param parallax
 *   parallax, if any
 */
final case class SiderealTracking(
  baseCoordinates: Coordinates,
  epoch:           Epoch,
  properMotion:    Option[ProperMotion],
  radialVelocity:  Option[RadialVelocity],
  parallax:        Option[Parallax]
) extends Tracking derives Eq {

  val at: TrackingAt = i =>
    epoch.untilInstant(i).flatMap(plusYears(_))

  /** Coordinates `elapsedYears` fractional epoch-years after `epoch`. */
  def plusYears(elapsedYears: Double): Option[Coordinates] =
    if (isConstant) baseCoordinates.some
    else
      SiderealTracking.coordinatesOn(
        baseCoordinates,
        epoch,
        properMotion.orEmpty,
        radialVelocity.getOrElse(RadialVelocity.Zero).toDoubleKilometersPerSecond,
        parallax.getOrElse(Parallax.Zero),
        elapsedYears
      )

  def isConstant: Boolean =
    properMotion.forall(_ === ProperMotion.Zero) &&
      radialVelocity.forall(_ === RadialVelocity.Zero) &&
      parallax.forall(_ === Parallax.Zero)


  // we have to implement this explicitly because Scala now just prints <function> :-\
  override def toString =
    s"SiderealTracking($baseCoordinates,$epoch,$properMotion,$radialVelocity,$parallax)"

}

object SiderealTracking extends SiderealTrackingOptics {
  import Constants.{AstronomicalUnit, TwoPi}

  def const(cs: Coordinates): SiderealTracking =
    SiderealTracking(cs, Epoch.J2000, none, none, none)

  /**
   * Coordinates corrected for proper motion
   * @param baseCoordinates
   *   base coordinates
   * @param epoch
   *   the epoch
   * @param properMotion
   *   proper velocity per epoch year
   * @param radialVelocity
   *   radial velocity (km/sec, positive if receding)
   * @param parallax
   *   parallax
   * @param elapsedYears
   *   elapsed time in epoch years
   * @return
   *   Coordinates corrected for proper motion. None for invalid Declination, e.g. +/-90
   */
  def coordinatesOn(
    baseCoordinates: Coordinates,
    epoch:           Epoch,
    properMotion:    ProperMotion,
    radialVelocity:  Double,
    parallax:        Parallax,
    elapsedYears:    Double
  ): Option[Coordinates] = {
    val result = coordinatesOnʹ(
      baseCoordinates.toRadians,
      epoch.scheme.daysInYear,
      properMotion.toRadians,
      radialVelocity,
      parallax.μas.value.value / 1000000.0,
      elapsedYears
    )
    result.map(Function.tupled(Coordinates.unsafeFromRadians))
  }

  // Some constants we need
  private val secsPerDay  = 86400.0
  private val auPerKm     = 1000.0 / AstronomicalUnit[Double].value
  private val radsPerAsec = Angle.arcseconds.reverseGet(1).toDoubleRadians

  // We need to do things with little vectors of doubles
  private type Vec2 = (Double, Double)
  private type Vec3 = (Double, Double, Double)

  // |+| gives us addition for VecN, but we also need scalar multiplication
  private implicit class Vec3Ops(private val a: Vec3) extends AnyVal {
    def *(d: Double): Vec3 =
      (a._1 * d, a._2 * d, a._3 * d)
  }

  /**
   * Coordinates corrected for proper motion
   * @param baseCoordinates
   *   base (ra, dec) in radians, [0 … 2π) and (-π/2 … π/2)
   * @param daysPerYear
   *   length of epoch year in fractonal days
   * @param properMotion
   *   proper velocity in (ra, dec) in radians per epoch year
   * @param radialVelocity
   *   radial velocity (km/sec, positive means away from earth)
   * @param parallax
   *   parallax in arcseconds (!)
   * @param elapsedYears
   *   elapsed time in epoch years
   * @return
   *   (ra, dec) in radians, corrected for proper motion
   */
  private def coordinatesOnʹ(
    baseCoordinates: Vec2,
    daysPerYear:     Double,
    properMotion:    Vec2,
    parallax:        Double,
    radialVelocity:  Double,
    elapsedYears:    Double
  ): Option[Vec2] = {
    // Break out our components
    val (ra, dec)    = baseCoordinates
    val (dRaʹ, dDec) = properMotion
    val cosDec       = cos(dec)
    val cosRa        = cos(ra)
    val sinDec       = sin(dec)
    val sinRa        = sin(ra)

    if (cosDec != 0) {

      // See: https://app.shortcut.com/lucuma/story/1388/proper-motion-calculation
      val dRa = dRaʹ / cosDec

      // Convert to cartesian
      val pos: Vec3 =
        (cosRa * cosDec, sinRa * cosDec, sinDec)

      // Change per year due to radial velocity and parallax. The units work out to asec/y.
      val dPos1: Vec3 =
        pos *
          daysPerYear *
          secsPerDay *
          radsPerAsec *
          auPerKm *
          radialVelocity *
          parallax

      // Change per year due to proper velocity
      val dPos2 = (
        -dRa * pos._2 - dDec * cosRa * sinDec,
        dRa * pos._1 - dDec * sinRa * sinDec,
        dDec * cosDec
      )

      // Our new position (still in polar coordinates). `|+|` here is scalar addition provided by
      // cats … unlike scalaz it does give you Semigroup[Double] even though it's not strictly lawful.
      val pʹ = pos |+| ((dPos1 |+| dPos2) * elapsedYears)

      // Back to spherical
      val (x, y, z) = pʹ
      val r         = hypot(x, y)
      val raʹ       = if (r === 0.0) 0.0 else atan2(y, x)
      val decʹ      = if (z === 0.0) 0.0 else atan2(z, r)
      val raʹʹ      = {
        // Normalize to [0 .. 2π)
        val rem = raʹ % TwoPi
        if (rem < 0.0) rem + TwoPi else rem
      }
      (raʹʹ, decʹ).some

    } else none
  }

  given Order[SiderealTracking] = {

    given Monoid[Order[SiderealTracking]] =
      Order.whenEqualMonoid[SiderealTracking]

    def order[A: Order](f: SiderealTracking => A): Order[SiderealTracking] =
      Order.by(f)

    // This could be done with:
    //
    //   Order.by(p => (p.baseCoordinates, p.epoch, ...))
    //
    // but that would always perform comparisons for all the fields (and all
    // their contained fields down to the leaves of the tree) all of the time.
    // The Monoid approach on the other hand will stop at the first difference.
    // This is premature optimization perhaps but it seems like it might make a
    // difference when sorting a long list of targets.

    order(_.baseCoordinates) |+|
      order(_.epoch) |+|
      order(_.properMotion) |+|
      order(_.radialVelocity) |+|
      order(_.parallax)

  }
}

trait SiderealTrackingOptics {

  /** @group Optics */
  val baseCoordinates: Lens[SiderealTracking, Coordinates] =
    Focus[SiderealTracking](_.baseCoordinates)

  /** @group Optics */
  val baseRa: Lens[SiderealTracking, RightAscension] =
    baseCoordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  val baseDec: Lens[SiderealTracking, Declination] =
    baseCoordinates.andThen(Coordinates.declination)

  /** @group Optics */
  val epoch: Lens[SiderealTracking, Epoch] =
    Focus[SiderealTracking](_.epoch)

  /** @group Optics */
  val properMotion: Lens[SiderealTracking, Option[ProperMotion]] =
    Focus[SiderealTracking](_.properMotion)

  /** @group Optics */
  val radialVelocity: Lens[SiderealTracking, Option[RadialVelocity]] =
    Focus[SiderealTracking](_.radialVelocity)

  /** @group Optics */
  val parallax: Lens[SiderealTracking, Option[Parallax]] =
    Focus[SiderealTracking](_.parallax)
}

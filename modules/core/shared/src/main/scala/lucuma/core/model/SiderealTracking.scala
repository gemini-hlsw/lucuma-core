// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import java.time.Instant

import scala.math.atan2
import scala.math.cos
import scala.math.hypot
import scala.math.sin

import cats._
import cats.syntax.all._
import lucuma.core.math._
import monocle.Lens
import monocle.macros.GenLens

/**
  * Time-parameterized coordinates, based on an observed position at some point in time (called
  * the `epoch`) and measured velocities in distance (`radialVelocity`; i.e., doppler shift) and
  * position (`properVelocity`) per year. Given this information we can compute the position at any
  * instant in time. The references below are ''extremely'' helpful, so do check them out if you're
  * trying to understand the implementation.
  * @see The pretty good [[https://en.wikipedia.org/wiki/Proper_motion wikipedia]] article
  * @see Astronomical Almanac 1984 [[https://babel.hathitrust.org/cgi/pt?id=uc1.b3754036;view=1up;seq=141 p.B39]]
  * @see Astronomy and Astrophysics 134 (1984) [[http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?bibcode=1984A%26A...134....1L&db_key=AST&page_ind=0&data_type=GIF&type=SCREEN_VIEW&classic=YES p.1-6]]
  * @param baseCoordinates observed coordinates at `epoch`
  * @param epoch           time of the base observation; typically `Epoch.J2000`
  * @param properVelocity  proper velocity '''per year''' in [[lucuma.core.math.RightAscension]] and [[lucuma.core.math.Declination]], if any
  * @param radialVelocity  radial velocity (km/y, positive if receding), if any
  * @param parallax        parallax, if any
  */
final case class SiderealTracking(
  catalogId:       Option[CatalogId],
  baseCoordinates: Coordinates,
  epoch:           Epoch,
  properVelocity:  Option[ProperVelocity],
  radialVelocity:  Option[RadialVelocity],
  parallax:        Option[Parallax]
) {

  def at(i: Instant): SiderealTracking =
    plusYears(epoch.untilInstant(i))

  /** Coordinates `elapsedYears` fractional epoch-years after `epoch`. */
  def plusYears(elapsedYears: Double): SiderealTracking =
    SiderealTracking(
      catalogId,
      SiderealTracking.properMotion(
        baseCoordinates,
        epoch,
        properVelocity.orEmpty,
        radialVelocity.getOrElse(RadialVelocity.Zero).toDoubleKilometersPerSecond,
        parallax.orEmpty,
        elapsedYears
      ),
      epoch.plusYears(elapsedYears),
      properVelocity,
      radialVelocity,
      parallax
    )

}

object SiderealTracking extends SiderealTrackingOptics {
  import Constants.{ AstronomicalUnit, TwoPi }

  def const(cs: Coordinates): SiderealTracking =
    SiderealTracking(none, cs, Epoch.J2000, None, None, None)

  /**
    * Proper motion correction in model units.
    * @param baseCoordinates base coordinates
    * @param epoch           the epoch
    * @param properVelocity  proper velocity per epoch year
    * @param radialVelocity  radial velocity (km/sec, positive if receding)
    * @param parallax        parallax
    * @param elapsedYears    elapsed time in epoch years
    * @return Coordinates corrected for proper motion
    */
  def properMotion(
    baseCoordinates: Coordinates,
    epoch:           Epoch,
    properVelocity:  ProperVelocity,
    radialVelocity:  Double,
    parallax:        Parallax,
    elapsedYears:    Double
  ): Coordinates = {
    val (ra, dec) = properMotionʹ(
      baseCoordinates.toRadians,
      epoch.scheme.lengthOfYear,
      properVelocity.toRadians,
      radialVelocity,
      parallax.μas.value / 1000000.0,
      elapsedYears
    )
    Coordinates.unsafeFromRadians(ra, dec)
  }

  // Some constants we need
  private val secsPerDay  = 86400.0
  private val auPerKm     = 1000.0 / AstronomicalUnit[Double].value
  private val radsPerAsec = Angle.arcseconds.reverseGet(1).toDoubleRadians

  // We need to do things with little vectors of doubles
  private type Vec2 = (Double, Double)
  private type Vec3 = (Double, Double, Double)

  // |+| gives us addition for VecN, but we also need scalar multiplication
  private implicit class Vec3Ops(a: Vec3) {
    def *(d: Double): Vec3 =
      (a._1 * d, a._2 * d, a._3 * d)
  }

  /**
    * Proper motion correction in base units.
    * @param baseCoordinates base (ra, dec) in radians, [0 … 2π) and (-π/2 … π/2)
    * @param daysPerYear     length of epoch year in fractonal days
    * @param properVelocity  proper velocity in (ra, dec) in radians per epoch year
    * @param radialVelocity  radial velocity (km/sec, positive means away from earth)
    * @param parallax        parallax in arcseconds (!)
    * @param elapsedYears    elapsed time in epoch years
    * @return (ra, dec) in radians, corrected for proper motion
    */
  // scalastyle:off method.length
  private def properMotionʹ(
    baseCoordinates: Vec2,
    daysPerYear:     Double,
    properVelocity:  Vec2,
    parallax:        Double,
    radialVelocity:  Double,
    elapsedYears:    Double
  ): Vec2 = {

    // Break out our components
    val (ra, dec)   = baseCoordinates
    val (dRa, dDec) = properVelocity

    // Convert to cartesian
    val pos: Vec3 = {
      val cd = cos(dec)
      (cos(ra) * cd, sin(ra) * cd, sin(dec))
    }

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
      -dRa * pos._2 - dDec * cos(ra) * sin(dec),
      dRa * pos._1 - dDec * sin(ra) * sin(dec),
      dDec * cos(dec)
    )

    // Our new position (still in polar coordinates). `|+|` here is scalar addition provided by
    // cats … unlike scalaz it does give you Semigroup[Double] even though it's not strictly lawful.
    val pʹ = pos |+| ((dPos1 |+| dPos2) * elapsedYears)

    // Back to spherical
    val (x, y, z) = pʹ
    val r         = hypot(x, y)
    val raʹ       = if (r === 0.0) 0.0 else atan2(y, x)
    val decʹ      = if (z === 0.0) 0.0 else atan2(z, r)
    val raʹʹ = {
      // Normalize to [0 .. 2π)
      val rem = raʹ % TwoPi
      if (rem < 0.0) rem + TwoPi else rem
    }
    (raʹʹ, decʹ)

  }
  // scalastyle:on method.length

  implicit val OrderSiderealTracking: Order[SiderealTracking] = {

    implicit val MonoidOrder: Monoid[Order[SiderealTracking]] =
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
      order(_.properVelocity) |+|
      order(_.radialVelocity) |+|
      order(_.parallax)

  }
}

trait SiderealTrackingOptics {

  /** @group Optics */
  val baseCoordinates: Lens[SiderealTracking, Coordinates] =
    GenLens[SiderealTracking](_.baseCoordinates)

  /** @group Optics */
  val catalogId: Lens[SiderealTracking, Option[CatalogId]] =
    GenLens[SiderealTracking](_.catalogId)

  /** @group Optics */
  val epoch: Lens[SiderealTracking, Epoch] =
    GenLens[SiderealTracking](_.epoch)

  /** @group Optics */
  val properVelocity: Lens[SiderealTracking, Option[ProperVelocity]] =
    GenLens[SiderealTracking](_.properVelocity)

  /** @group Optics */
  val radialVelocity: Lens[SiderealTracking, Option[RadialVelocity]] =
    GenLens[SiderealTracking](_.radialVelocity)

  /** @group Optics */
  val parallax: Lens[SiderealTracking, Option[Parallax]] =
    GenLens[SiderealTracking](_.parallax)

}

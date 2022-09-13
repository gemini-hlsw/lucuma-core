// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import lucuma.core.math.*
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens

import java.time.Instant

/** A coordinate along with a rate of change in RA and Dec for some time unit
  *
  * @param coord coordinates
  * @param delta rate of change in RA and dec, where the delta(RA)/time has been
  *              multiplied by the cosine of the dec
  */
sealed trait TrackedCoordinates {
  def coord: Coordinates
}

/** A coordinate along with a rate of change in RA and Dec for some time unit,
  * expressed as an offset in p and q.  In reality the velocity information
  * comes from horizons and is always arcseconds per hour in horizons data.
  *
  * @param coord coordinates
  * @param delta rate of change in RA and dec, where the delta(RA)/time has been
  *              multiplied by the cosine of the dec
  */
case class EphemerisCoordinates(
                   override val coord: Coordinates,
                   val delta: Offset /* per time unit */)
    extends TrackedCoordinates {

  /** Interpolates the position and rate of change at a point between this
    * coordinate and the given coordinate.
    */
  def interpolate(that: EphemerisCoordinates, f: Double): EphemerisCoordinates = {
    def interpolateAngle(a: Angle, b: Angle): Angle =
      Angle.fromMicroarcseconds(
        (Angle.signedMicroarcseconds.get(a).toDouble * (1 - f) + Angle.signedMicroarcseconds.get( b) * f).round
      )

    val coordʹ = coord.interpolate(that.coord, f)
    val pʹ     = interpolateAngle(delta.p.toAngle, that.delta.p.toAngle)
    val qʹ     = interpolateAngle(delta.q.toAngle, that.delta.q.toAngle)

    EphemerisCoordinates(coordʹ, Offset(Offset.P(pʹ), Offset.Q(qʹ)))
  }

}

object EphemerisCoordinates extends EphemerisCoordinatesOptics {

  val Zero: EphemerisCoordinates =
    EphemerisCoordinates(Coordinates.Zero, Offset.Zero)

  /** @group Typeclass Instances */
  implicit val EphemerisCoordinatesEqual: Eq[EphemerisCoordinates] =
    Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit val ShowEphemerisCoordinates: Show[EphemerisCoordinates] =
    Show.fromToString

}

trait EphemerisCoordinatesOptics {

  /** @group Optics */
  val coordinates: Lens[EphemerisCoordinates, Coordinates] =
    Focus[EphemerisCoordinates](_.coord)

  /** @group Optics */
  val delta: Lens[EphemerisCoordinates, Offset] =
    Focus[EphemerisCoordinates](_.delta)

  /** @group Optics */
  val rightAscension: Lens[EphemerisCoordinates, RightAscension] =
    coordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  val declination: Lens[EphemerisCoordinates, Declination] =
    coordinates.andThen(Coordinates.declination)

  /** @group Optics */
  val deltaP: Lens[EphemerisCoordinates, Offset.P] =
    delta.andThen(Offset.p)

  /** @group Optics */
  val deltaQ: Lens[EphemerisCoordinates, Offset.Q] =
    delta.andThen(Offset.q)

}

case class SiderealCoordinates(override val coord: Coordinates) extends TrackedCoordinates

object SiderealCoordinates extends SiderealCoordinatesOptics {
  given Eq[SiderealCoordinates] = Eq.by(_.coord)
}

trait SiderealCoordinatesOptics {
  /** @group Optics */
  val coordinates: Lens[SiderealCoordinates, Coordinates] =
    Focus[SiderealCoordinates](_.coord)

  /** @group Optics */
  val rightAscension: Lens[SiderealCoordinates, RightAscension] =
    coordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  val declination: Lens[SiderealCoordinates, Declination] =
    coordinates.andThen(Coordinates.declination)
}

/**
 * Implementors can calculate the coordinates for a given object at a certain timestamp
 */
trait Tracking {
  def at(i: Instant): Option[TrackedCoordinates]
  def atEpoch: TrackedCoordinates
}

object Tracking {
  def const(coord: Coordinates): Tracking = new Tracking {
    def at(i: Instant): Option[TrackedCoordinates] = Some(SiderealCoordinates(coord))
    def atEpoch: TrackedCoordinates = SiderealCoordinates(coord)
  }
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import scala.collection.immutable.SortedMap

import cats._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math._
import monocle.Lens
import monocle.Optional
import monocle.Traversal
import monocle.function.Each.mapEach
import monocle.function.FilterIndex
import monocle.Focus
import monocle.std.either._

/** A target of observation. */
final case class Target(
  name:       NonEmptyString,
  track:      Either[EphemerisKey, SiderealTracking],
  magnitudes: SortedMap[MagnitudeBand, Magnitude]
)

object Target extends WithId with TargetOptics {
  protected val idTag = 't'

  implicit val TargetEq: Eq[Target] =
    Eq.by(x => (x.name, x.track, x.magnitudes))

  /**
    * A target order based on tracking information.  For sidereal targets this
    * roughly means by base coordinate without applying proper motion.  For
    * non-sidereal this means by `EphemerisKey`.
    *
   * Not implicit.
    */
  val TargetTrackOrder: Order[Target] =
    Order.by(t => (t.track, t.name))

  /**
    * Targets ordered by name first and then tracking information.
    *
   * Not implicit.
    */
  val TargetNameOrder: Order[Target] =
    Order.by(t => (t.name.value, t.track))

}

trait TargetOptics {

  /** @group Optics */
  lazy val name: Lens[Target, NonEmptyString] =
    Focus[Target](_.name)

  /** @group Optics */
  lazy val track: Lens[Target, Either[EphemerisKey, SiderealTracking]] =
    Focus[Target](_.track)

  /** @group Optics */
  lazy val ephemerisKey: Optional[Target, EphemerisKey] =
    track.andThen(stdLeft[EphemerisKey, SiderealTracking])

  /** @group Optics */
  lazy val siderealTracking: Optional[Target, SiderealTracking] =
    track.andThen(stdRight[EphemerisKey, SiderealTracking])

  /** @group Optics */
  lazy val magnitudes: Lens[Target, SortedMap[MagnitudeBand, Magnitude]] =
    Focus[Target](_.magnitudes)

  /** @group Optics */
  lazy val magnitudesT: Traversal[Target, Magnitude] =
    magnitudes.andThen(mapEach(Order[MagnitudeBand]).each)

  /** @group Optics */
  def magnitudeIn(b: MagnitudeBand): Traversal[Target, Magnitude] =
    magnitudes
      .andThen(
        FilterIndex
          .sortedMapFilterIndex(Order[MagnitudeBand])
          .filterIndex(_ === b)
      )

  /** @group Optics */
  lazy val parallax: Optional[Target, Option[Parallax]] =
    siderealTracking.andThen(SiderealTracking.parallax)

  /** @group Optics */
  lazy val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    siderealTracking.andThen(SiderealTracking.radialVelocity)

  /** @group Optics */
  lazy val baseCoordinates: Optional[Target, Coordinates] =
    siderealTracking.andThen(SiderealTracking.baseCoordinates)

  /** @group Optics */
  lazy val baseRA: Optional[Target, RightAscension] =
    baseCoordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  lazy val baseDec: Optional[Target, Declination] =
    baseCoordinates.andThen(Coordinates.declination)

  /** @group Optics */
  lazy val properMotion =
    siderealTracking.andThen(SiderealTracking.properMotion.some)

  /** @group Optics */
  lazy val properMotionRA = properMotion.andThen(ProperMotion.ra)

  /** @group Optics */
  lazy val properMotionDec = properMotion.andThen(ProperMotion.dec)

}

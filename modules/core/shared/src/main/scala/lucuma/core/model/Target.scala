// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import scala.collection.immutable.SortedMap

import cats._
import cats.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math._
import monocle.Lens
import monocle.Optional
import monocle.Traversal
import monocle.function.Each.mapEach
import monocle.function.FilterIndex
import monocle.macros.GenLens
import monocle.std.either._
import monocle.std.option

/** A target of observation. */
final case class Target(
  name:       NonEmptyString,
  catalogId:  Option[NonEmptyString],
  track:      Either[EphemerisKey, ProperMotion],
  magnitudes: SortedMap[MagnitudeBand, Magnitude]
)

object Target extends TargetOptics {

  /** Target identifier. */
  final case class Id(toInt: Int) extends AnyVal

  object Id {

    /** Ids ordered by wrapped integer value. */
    implicit val IdOrder: Order[Id] =
      Order.by(_.toInt)
  }

  implicit val TargetEq: Eq[Target] =
    Eq.by(x => (x.name, x.catalogId, x.track, x.magnitudes))

  /** A target order based on tracking information.  For sidereal targets this
    * roughly means by base coordinate without applying proper motion.  For
    * non-sidereal this means by `EphemerisKey`.
    *
    * Not implicit.
    */
  val TargetTrackOrder: Order[Target] =
    Order.by(t => (t.track, t.name, t.catalogId))

  /** Targets ordered by name first and then tracking information.
    *
    * Not implicit.
    */
  val TargetNameOrder: Order[Target] =
    Order.by(t => (t.name.value, t.catalogId, t.track))

}

trait TargetOptics {

  /** @group Optics */
  lazy val name: Lens[Target, NonEmptyString] =
    GenLens[Target](_.name)

  /** @group Optics */
  lazy val catalogId: Lens[Target, Option[NonEmptyString]] =
    GenLens[Target](_.catalogId)

  /** @group Optics */
  lazy val track: Lens[Target, Either[EphemerisKey, ProperMotion]] =
    GenLens[Target](_.track)

  /** @group Optics */
  lazy val ephemerisKey: Optional[Target, EphemerisKey] =
    track.composePrism(stdLeft)

  /** @group Optics */
  lazy val properMotion: Optional[Target, ProperMotion] =
    track.composePrism(stdRight)

  /** @group Optics */
  lazy val magnitudes: Lens[Target, SortedMap[MagnitudeBand, Magnitude]] =
    GenLens[Target](_.magnitudes)

  /** @group Optics */
  lazy val magnitudesT: Traversal[Target, Magnitude] =
    magnitudes.composeTraversal(mapEach(Order[MagnitudeBand]).each)

  /** @group Optics */
  def magnitudeIn(b: MagnitudeBand): Traversal[Target, Magnitude] =
    magnitudes
      .composeTraversal(
        FilterIndex
          .sortedMapFilterIndex(Order[MagnitudeBand])
          .filterIndex(_ === b)
      )

  /** @group Optics */
  lazy val parallax: Optional[Target, Option[Parallax]] =
    properMotion.composeLens(ProperMotion.parallax)

  /** @group Optics */
  lazy val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    properMotion.composeLens(ProperMotion.radialVelocity)

  /** @group Optics */
  lazy val baseCoordinates: Optional[Target, Coordinates] =
    properMotion.composeLens(ProperMotion.baseCoordinates)

  /** @group Optics */
  lazy val baseRA: Optional[Target, RightAscension] =
    baseCoordinates.composeLens(Coordinates.rightAscension)

  /** @group Optics */
  lazy val baseDec: Optional[Target, Declination] =
    baseCoordinates.composeLens(Coordinates.declination)

  /** @group Optics */
  lazy val properVelocity =
    properMotion.composeOptional(ProperMotion.properVelocity.composePrism(option.some))

  /** @group Optics */
  lazy val properVelocityRA = properVelocity.composeLens(ProperVelocity.ra)

  /** @group Optics */
  lazy val properVelocityDec = properVelocity.composeLens(ProperVelocity.dec)

}

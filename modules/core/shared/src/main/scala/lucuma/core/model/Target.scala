// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.Band
import lucuma.core.math._
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism

import scala.collection.immutable.SortedMap

/** A target of observation. */
sealed trait Target extends Product with Serializable {
  def name: NonEmptyString
  def brightnesses: SortedMap[Band, TargetBrightness]
  def angularSize: Option[AngularSize]
}

final case class SiderealTarget(
  name:         NonEmptyString,
  tracking:     SiderealTracking,
  brightnesses: SortedMap[Band, TargetBrightness],
  angularSize:  Option[AngularSize]
) extends Target

object SiderealTarget extends SiderealTargetOptics {
  implicit val SiderealTargetTargetEq: Eq[SiderealTarget] =
    Eq.by(x => (x.name, x.tracking, x.brightnesses))

  /**
   * A sidereal target order based on tracking information, which roughly means by base coordinate
   * without applying proper motion.
   *
   * Not implicit.
   */
  val TrackOrder: Order[SiderealTarget] =
    Order.by(t => (t.tracking, t.name, t.brightnesses.toList))

  /**
   * Sidereal targets ordered by name first and then tracking information.
   *
   * Not implicit.
   */
  val NameOrder: Order[SiderealTarget] =
    Order.by(t => (t.name, t.tracking, t.brightnesses.toList))
}

final case class NonsiderealTarget(
  name:         NonEmptyString,
  ephemerisKey: EphemerisKey,
  brightnesses: SortedMap[Band, TargetBrightness],
  angularSize:  Option[AngularSize]
) extends Target

object NonsiderealTarget extends NonsiderealTargetOptics {
  implicit val NonsiderealTargetTargetEq: Eq[NonsiderealTarget] =
    Eq.by(x => (x.name, x.ephemerisKey, x.brightnesses))

  /**
   * A nonsidereal target order based on ephemeris key.
   *
   * Not implicit.
   */
  val TrackOrder: Order[NonsiderealTarget] =
    Order.by(t => (t.ephemerisKey, t.name, t.brightnesses.toList))

  /**
   * Nonsidereal targets ordered by name first and then ephemeris key.
   *
   * Not implicit.
   */
  val NameOrder: Order[NonsiderealTarget] =
    Order.by(t => (t.name, t.ephemerisKey, t.brightnesses.toList))
}

object Target extends WithId('t') with TargetOptics {

  implicit val TargetEq: Eq[Target] = Eq.instance {
    case (a @ SiderealTarget(_, _, _, _), b @ SiderealTarget(_, _, _, _))       => a === b
    case (a @ NonsiderealTarget(_, _, _, _), b @ NonsiderealTarget(_, _, _, _)) => a === b
    case _                                                                      => false
  }

  /**
   * A target order based on tracking information. For sidereal targets this roughly means by base
   * coordinate without applying proper motion. For non-sidereal this means by `EphemerisKey`.
   *
   * Not implicit.
   */
  val TrackOrder: Order[Target] =
    Order.from {
      case (a @ SiderealTarget(_, _, _, _), b @ SiderealTarget(_, _, _, _))       =>
        SiderealTarget.TrackOrder.compare(a, b)
      case (a @ NonsiderealTarget(_, _, _, _), b @ NonsiderealTarget(_, _, _, _)) =>
        NonsiderealTarget.TrackOrder.compare(a, b)
      case (NonsiderealTarget(_, _, _, _), _)                                     => -1
      case _                                                                      => 1
    }

  /**
   * Targets ordered by name first and then tracking information.
   *
   * Not implicit.
   */
  val NameOrder: Order[Target] =
    Order.from {
      case (a @ SiderealTarget(_, _, _, _), b @ SiderealTarget(_, _, _, _))       =>
        SiderealTarget.NameOrder.compare(a, b)
      case (a @ NonsiderealTarget(_, _, _, _), b @ NonsiderealTarget(_, _, _, _)) =>
        NonsiderealTarget.NameOrder.compare(a, b)
      case (NonsiderealTarget(_, _, _, _), _)                                     => -1
      case _                                                                      => 1
    }
}

trait SiderealTargetOptics { this: SiderealTarget.type =>

  /** @group Optics */
  val name: Lens[SiderealTarget, NonEmptyString] =
    Focus[SiderealTarget](_.name)

  /** @group Optics */
  val tracking: Lens[SiderealTarget, SiderealTracking] =
    Focus[SiderealTarget](_.tracking)

  /** @group Optics */
  val brightnesses: Lens[SiderealTarget, SortedMap[Band, TargetBrightness]] =
    Focus[SiderealTarget](_.brightnesses)

  /** @group Optics */
  val brightnessesT: Traversal[SiderealTarget, TargetBrightness] =
    brightnesses.each

  /** @group Optics */
  def brightnessIn(b: Band): Traversal[SiderealTarget, TargetBrightness] =
    brightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val parallax: Lens[SiderealTarget, Option[Parallax]] =
    tracking.andThen(SiderealTracking.parallax)

  /** @group Optics */
  val radialVelocity: Lens[SiderealTarget, Option[RadialVelocity]] =
    tracking.andThen(SiderealTracking.radialVelocity)

  /** @group Optics */
  val baseCoordinates: Lens[SiderealTarget, Coordinates] =
    tracking.andThen(SiderealTracking.baseCoordinates)

  /** @group Optics */
  val baseRA: Lens[SiderealTarget, RightAscension] =
    baseCoordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  val baseDec: Lens[SiderealTarget, Declination] =
    baseCoordinates.andThen(Coordinates.declination)

  /** @group Optics */
  val catalogId: Lens[SiderealTarget, Option[CatalogId]] =
    tracking.andThen(SiderealTracking.catalogId)

  /** @group Optics */
  val epoch: Lens[SiderealTarget, Epoch] =
    tracking.andThen(SiderealTracking.epoch)

  /** @group Optics */
  val properMotion: Lens[SiderealTarget, Option[ProperMotion]] =
    tracking.andThen(SiderealTracking.properMotion)

  /** @group Optics */
  val properMotionRA: Optional[SiderealTarget, ProperMotion.RA] =
    properMotion.some.andThen(ProperMotion.ra)

  /** @group Optics */
  val properMotionDec: Optional[SiderealTarget, ProperMotion.Dec] =
    properMotion.some.andThen(ProperMotion.dec)
}

trait NonsiderealTargetOptics { this: NonsiderealTarget.type =>

  /** @group Optics */
  val name: Lens[NonsiderealTarget, NonEmptyString] =
    Focus[NonsiderealTarget](_.name)

  /** @group Optics */
  val ephemerisKey: Lens[NonsiderealTarget, EphemerisKey] =
    Focus[NonsiderealTarget](_.ephemerisKey)

  /** @group Optics */
  val brightnesses: Lens[NonsiderealTarget, SortedMap[Band, TargetBrightness]] =
    Focus[NonsiderealTarget](_.brightnesses)

  /** @group Optics */
  val brightnessesT: Traversal[NonsiderealTarget, TargetBrightness] =
    brightnesses.each

  /** @group Optics */
  def brightnessIn(b: Band): Traversal[NonsiderealTarget, TargetBrightness] =
    brightnesses.filterIndex((a: Band) => a === b)
}

trait TargetOptics { this: Target.type =>

  /** @group Optics */
  val sidereal: Prism[Target, SiderealTarget] = GenPrism[Target, SiderealTarget]

  /** @group Optics */
  val nonsidereal: Prism[Target, NonsiderealTarget] = GenPrism[Target, NonsiderealTarget]

  /** @group Optics */
  val name: Lens[Target, NonEmptyString] =
    Lens[Target, NonEmptyString](_.name)(v => {
      case t @ SiderealTarget(_, _, _, _)    => SiderealTarget.name.replace(v)(t)
      case t @ NonsiderealTarget(_, _, _, _) => NonsiderealTarget.name.replace(v)(t)
    })

  /** @group Optics */
  val ephemerisKey: Optional[Target, EphemerisKey] =
    nonsidereal.andThen(NonsiderealTarget.ephemerisKey)

  /** @group Optics */
  val siderealTracking: Optional[Target, SiderealTracking] =
    sidereal.andThen(SiderealTarget.tracking)

  /** @group Optics */
  val brightnesses: Lens[Target, SortedMap[Band, TargetBrightness]] =
    Lens[Target, SortedMap[Band, TargetBrightness]](_.brightnesses)(v => {
      case t @ SiderealTarget(_, _, _, _)    => SiderealTarget.brightnesses.replace(v)(t)
      case t @ NonsiderealTarget(_, _, _, _) => NonsiderealTarget.brightnesses.replace(v)(t)
    })

  /** @group Optics */
  val brightnessesT: Traversal[Target, TargetBrightness] =
    brightnesses.each

  /** @group Optics */
  def brightnessIn(b: Band): Traversal[Target, TargetBrightness] =
    brightnesses.filterIndex((a: Band) => a === b)

  /** @group Optics */
  val parallax: Optional[Target, Option[Parallax]] =
    siderealTracking.andThen(SiderealTracking.parallax)

  /** @group Optics */
  val radialVelocity: Optional[Target, Option[RadialVelocity]] =
    siderealTracking.andThen(SiderealTracking.radialVelocity)

  /** @group Optics */
  val baseCoordinates: Optional[Target, Coordinates] =
    siderealTracking.andThen(SiderealTracking.baseCoordinates)

  /** @group Optics */
  val baseRA: Optional[Target, RightAscension] =
    baseCoordinates.andThen(Coordinates.rightAscension)

  /** @group Optics */
  val baseDec: Optional[Target, Declination] =
    baseCoordinates.andThen(Coordinates.declination)

  /** @group Optics */
  val catalogId: Optional[Target, Option[CatalogId]] =
    sidereal.andThen(SiderealTarget.catalogId)

  /** @group Optics */
  val epoch: Optional[Target, Epoch] =
    sidereal.andThen(SiderealTarget.epoch)

  /** @group Optics */
  val properMotion: Optional[Target, Option[ProperMotion]] =
    sidereal.andThen(SiderealTarget.properMotion)

  /** @group Optics */
  val properMotionRA: Optional[Target, ProperMotion.RA] =
    sidereal.andThen(SiderealTarget.properMotionRA)

  /** @group Optics */
  val properMotionDec: Optional[Target, ProperMotion.Dec] =
    sidereal.andThen(SiderealTarget.properMotionDec)
}

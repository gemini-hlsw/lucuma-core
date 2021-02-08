// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum._
import monocle.{ Lens, Optional }
import monocle.macros.GenLens

final case class ConstraintSet(
  name:            NonEmptyString,
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRange
)

object ConstraintSet extends WithId with ConstraintSetOptics {
  protected val idTag = 'c'

  implicit val constraintSetEq: Eq[ConstraintSet] = Eq.fromUniversalEquals
}

trait ConstraintSetOptics {

  /** @group Optics */
  lazy val name: Lens[ConstraintSet, NonEmptyString] =
    GenLens[ConstraintSet](_.name)

  /** @group Optics */
  lazy val imageQuality: Lens[ConstraintSet, ImageQuality] =
    GenLens[ConstraintSet](_.imageQuality)

  /** @group Optics */
  lazy val cloudExtinction: Lens[ConstraintSet, CloudExtinction] =
    GenLens[ConstraintSet](_.cloudExtinction)

  /** @group Optics */
  lazy val skyBackground: Lens[ConstraintSet, SkyBackground] =
    GenLens[ConstraintSet](_.skyBackground)

  /** @group Optics */
  lazy val waterVapor: Lens[ConstraintSet, WaterVapor] =
    GenLens[ConstraintSet](_.waterVapor)

  /** @group Optics */
  lazy val elevationRange: Lens[ConstraintSet, ElevationRange] =
    GenLens[ConstraintSet](_.elevationRange)

  /** @group Optics */
  lazy val airmass: Optional[ConstraintSet, AirmassRange] =
    elevationRange.composePrism(ElevationRange.airmass)

  /** @group Optics */
  lazy val hourAngle: Optional[ConstraintSet, HourAngleRange] =
    elevationRange.composePrism(ElevationRange.hourAngle)
}

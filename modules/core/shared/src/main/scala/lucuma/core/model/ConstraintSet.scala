// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import eu.timepit.refined.cats._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import monocle.Focus
import monocle.Lens
import monocle.Optional

/** Constraints for an observation. */
final case class ConstraintSet(
  imageQuality:    ImageQuality,
  cloudExtinction: CloudExtinction,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRange
)

object ConstraintSet {

  /** @group Typeclass Instances */
  implicit val eqConstraintsSet: Eq[ConstraintSet] = Eq.by(cs =>
    (cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor, cs.elevationRange)
  )

  /** @group Optics */
  val imageQuality: Lens[ConstraintSet, ImageQuality] =
    Focus[ConstraintSet](_.imageQuality)

  /** @group Optics */
  val cloudExtinction: Lens[ConstraintSet, CloudExtinction] =
    Focus[ConstraintSet](_.cloudExtinction)

  /** @group Optics */
  val skyBackground: Lens[ConstraintSet, SkyBackground] =
    Focus[ConstraintSet](_.skyBackground)

  /** @group Optics */
  val waterVapor: Lens[ConstraintSet, WaterVapor] =
    Focus[ConstraintSet](_.waterVapor)

  /** @group Optics */
  val elevationRange: Lens[ConstraintSet, ElevationRange] =
    Focus[ConstraintSet](_.elevationRange)

  /** @group Optics */
  lazy val airMass: Optional[ConstraintSet, ElevationRange.AirMass] =
    elevationRange.andThen(ElevationRange.airMass)

  /** @group Optics */
  lazy val airMassMin: Optional[ConstraintSet, ElevationRange.AirMass.DecimalValue] =
    airMass.andThen(ElevationRange.AirMass.min)

  /** @group Optics */
  lazy val airMassMax: Optional[ConstraintSet, ElevationRange.AirMass.DecimalValue] =
    airMass.andThen(ElevationRange.AirMass.max)

  /** @group Optics */
  lazy val hourAngle: Optional[ConstraintSet, ElevationRange.HourAngle] =
    elevationRange.andThen(ElevationRange.hourAngle)

  /** @group Optics */
  lazy val hourAngleMin: Optional[ConstraintSet, ElevationRange.HourAngle.DecimalHour] =
    hourAngle.andThen(ElevationRange.HourAngle.minHours)

  /** @group Optics */
  lazy val hourAngleMax: Optional[ConstraintSet, ElevationRange.HourAngle.DecimalHour] =
    hourAngle.andThen(ElevationRange.HourAngle.maxHours)
}

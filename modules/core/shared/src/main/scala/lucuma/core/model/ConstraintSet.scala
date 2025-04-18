// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import lucuma.core.conditions.conditionsLikelihood
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens
import monocle.Optional

/** Constraints for an observation. */
case class ConstraintSet(
  imageQuality:    ImageQuality.Preset,
  cloudExtinction: CloudExtinction.Preset,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRange
):
  /** Calculations of likelihood of occurrence of observing conditions
    * Taken from: https://github.com/andrewwstephens/pyexplore/blob/main/test/percentile.py
    */
  def likelihood(wavelength: Wavelength, dec: Declination, site: Site): IntCentiPercent =
    conditionsLikelihood(skyBackground, cloudExtinction.toCloudExtinction, waterVapor, imageQuality.toImageQuality, wavelength, dec, site)

object ConstraintSet {

  /** @group Typeclass Instances */
  given Eq[ConstraintSet] = Eq.by(cs =>
    (cs.imageQuality, cs.cloudExtinction, cs.skyBackground, cs.waterVapor, cs.elevationRange)
  )

  /** @group Optics */
  val imageQuality: Lens[ConstraintSet, ImageQuality.Preset] =
    Focus[ConstraintSet](_.imageQuality)

  /** @group Optics */
  val cloudExtinction: Lens[ConstraintSet, CloudExtinction.Preset] =
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
  lazy val airMass: Optional[ConstraintSet, ElevationRange.ByAirMass] =
    elevationRange.andThen(ElevationRange.airMass)

  /** @group Optics */
  lazy val airMassMin: Optional[ConstraintSet, AirMassBound] =
    airMass.andThen(ElevationRange.ByAirMass.min)

  /** @group Optics */
  lazy val airMassMax: Optional[ConstraintSet, AirMassBound] =
    airMass.andThen(ElevationRange.ByAirMass.max)

  /** @group Optics */
  lazy val hourAngle: Optional[ConstraintSet, ElevationRange.ByHourAngle] =
    elevationRange.andThen(ElevationRange.hourAngle)

  /** @group Optics */
  lazy val hourAngleMin: Optional[ConstraintSet, HourAngleBound] =
    hourAngle.andThen(ElevationRange.ByHourAngle.minHours)

  /** @group Optics */
  lazy val hourAngleMax: Optional[ConstraintSet, HourAngleBound] =
    hourAngle.andThen(ElevationRange.ByHourAngle.maxHours)
}

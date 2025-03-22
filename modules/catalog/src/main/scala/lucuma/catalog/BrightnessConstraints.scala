// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.Order
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue

/**
 * Constrain a target if a brightness is fainter than a threshold
 */
case class FaintnessConstraint(brightness: BrightnessValue) derives Order

/**
 * Constrain a target's if a brightness is brighter than a threshold
 */
case class SaturationConstraint(brightness: BrightnessValue) derives Order

/**
 * Describes constraints for the brightness of a target
 */
case class BrightnessConstraints(
  searchBands:          BandsList,
  faintnessConstraint:  FaintnessConstraint,
  saturationConstraint: Option[SaturationConstraint]
) derives Eq {
  def contains(band: Band, brightness: BrightnessValue): Boolean =
    searchBands.bands.contains(band) &&
      faintnessConstraint.brightness >= brightness &&
      saturationConstraint.forall(_.brightness <= brightness)

  def ∪(that: BrightnessConstraints): BrightnessConstraints =
    BrightnessConstraints(
      searchBands ∪ that.searchBands,
      FaintnessConstraint(
        faintnessConstraint.brightness.max(that.faintnessConstraint.brightness)
      ),
      (this.saturationConstraint, that.saturationConstraint) match {
        case (a @ Some(_), None) => a
        case (None, a @ Some(_)) => a
        case (Some(a), Some(b))  =>
          SaturationConstraint(a.brightness.min(b.brightness)).some
        case _                   => none
      }
    )
}

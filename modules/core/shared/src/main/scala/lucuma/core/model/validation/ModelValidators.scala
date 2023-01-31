// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.validation

import eu.timepit.refined.auto._
import lucuma.core.model.ElevationRange
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.validation._
import lucuma.refined._

object ModelValidators {
  private val airMassErrorMsg   =
    f"Must be ${ElevationRange.AirMass.MinValue.toDouble}%.1f to ${ElevationRange.AirMass.MaxValue.toDouble}%.1f"
  private val hourAngleErrorMsg =
    f"Must be ${ElevationRange.HourAngle.MinHour.toDouble}%.1f to ${ElevationRange.HourAngle.MaxHour.toDouble}%.1f"

  val airMassElevationRangeValidWedge: InputValidWedge[ElevationRange.AirMass.DecimalValue] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(
        ValidSplitEpi
          .forRefined[String, BigDecimal, ElevationRange.AirMass.Value](_ => airMassErrorMsg)
          .toErrorsValidSplitEpiUnsafe
      )

  val hourAngleElevationRangeValidWedge: InputValidWedge[ElevationRange.HourAngle.DecimalHour] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(
        ValidSplitEpi
          .forRefined[String, BigDecimal, ElevationRange.HourAngle.Hour](_ => hourAngleErrorMsg)
          .toErrorsValidSplitEpiUnsafe
      )
}

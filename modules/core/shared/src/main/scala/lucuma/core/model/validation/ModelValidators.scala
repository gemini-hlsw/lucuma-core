// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.validation

import lucuma.core.model.AirMass
import lucuma.core.model.AirMassBound
import lucuma.core.model.HourAngleBound
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.refined.given
import lucuma.core.validation.*
import lucuma.refined.*

object ModelValidators {
  private val airMassErrorMsg: String   =
    f"Must be ${AirMassBound.Min.toBigDecimal.toDouble}%.1f to ${AirMassBound.Max.toBigDecimal.toDouble}%.1f"
  private val hourAngleErrorMsg: String =
    f"Must be ${HourAngleBound.Min.value}%.1f to ${HourAngleBound.Max.value}%.1f"

  val AirMassValidate: ValidSplitEpi[String, BigDecimal, AirMass] = 
    ValidSplitEpi
      .forRefined[String, BigDecimal, AirMass.Predicate](_ => airMassErrorMsg)
      .andThen(AirMass.Value.reverse)

  val AirMassConstraintValidate: ValidSplitEpi[String, AirMass, AirMassBound] = 
    ValidSplitEpi
      .forRefined[String, AirMass, AirMassBound.Predicate](_ => airMassErrorMsg)
      .andThen(AirMassBound.Value.reverse)

  val AirMassElevationRangeValidWedge: InputValidWedge[AirMassBound] = 
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(AirMassValidate.toErrorsValidSplitEpiUnsafe)
      .andThen(AirMassConstraintValidate.toErrorsValidSplitEpiUnsafe)

  val HourAngleConstraintValidate: ValidSplitEpi[String, BigDecimal, HourAngleBound] =
    ValidSplitEpi
     .forRefined[String, BigDecimal, HourAngleBound.Predicate](_ => hourAngleErrorMsg)
    .andThen(HourAngleBound.Value.reverse)

  val HourAngleElevationRangeValidWedge: InputValidWedge[HourAngleBound] = 
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(HourAngleConstraintValidate.toErrorsValidSplitEpiUnsafe)
}

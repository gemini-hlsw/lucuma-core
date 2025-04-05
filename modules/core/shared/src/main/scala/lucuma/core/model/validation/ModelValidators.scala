// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.validation

import lucuma.core.model.AirMass
import lucuma.core.model.AirMassConstraint
import lucuma.core.model.HourAngleConstraint
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.refined.given
import lucuma.core.validation.*
import lucuma.refined.*

object ModelValidators {
  private val airMassErrorMsg: String   =
    f"Must be ${AirMassConstraint.Min.value.value.value.value.toDouble}%.1f to ${AirMassConstraint.Max.value.value.value.value.toDouble}%.1f"
  private val hourAngleErrorMsg: String =
    f"Must be ${HourAngleConstraint.Min.value}%.1f to ${HourAngleConstraint.Max.value}%.1f"

  // TODO TEST NEW OPTICS!!!!

  val AirMassValidate: ValidSplitEpi[String, BigDecimal, AirMass] = 
    ValidSplitEpi
      .forRefined[String, BigDecimal, AirMass.Predicate](_ => airMassErrorMsg)
      .andThen(AirMass.Value.reverse)

  val AirMassConstraintValidate: ValidSplitEpi[String, AirMass, AirMassConstraint] = 
    ValidSplitEpi
      .forRefined[String, AirMass, AirMassConstraint.Predicate](_ => airMassErrorMsg)
      .andThen(AirMassConstraint.Value.reverse)

  val AirMassElevationRangeValidWedge: InputValidWedge[AirMassConstraint] = 
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(AirMassValidate.toErrorsValidSplitEpiUnsafe)
      .andThen(AirMassConstraintValidate.toErrorsValidSplitEpiUnsafe)

  val HourAngleConstraintValidate: ValidSplitEpi[String, BigDecimal, HourAngleConstraint] =
    ValidSplitEpi
     .forRefined[String, BigDecimal, HourAngleConstraint.Predicate](_ => hourAngleErrorMsg)
    .andThen(HourAngleConstraint.Value.reverse)

  val HourAngleElevationRangeValidWedge: InputValidWedge[HourAngleConstraint] = 
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(HourAngleConstraintValidate.toErrorsValidSplitEpiUnsafe)
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.validation

import lucuma.core.model.AirMass
import lucuma.core.model.AirMassBound
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Extinction
import lucuma.core.model.HourAngleBound
import lucuma.core.model.ImageQuality
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.refined.given
import lucuma.core.syntax.validation.*
import lucuma.core.validation.*
import lucuma.refined.*

object ModelValidators {
  private val AirMassErrorMsg: String   =
    f"Must be ${AirMassBound.Min.toBigDecimal.toDouble}%.1f to ${AirMassBound.Max.toBigDecimal.toDouble}%.1f"
  private val HourAngleErrorMsg: String =
    f"Must be ${HourAngleBound.Min.value}%.1f to ${HourAngleBound.Max.value}%.1f"

  val AirMassValidate: ValidSplitEpi[String, BigDecimal, AirMass] =
    ValidSplitEpi
      .forRefined[String, BigDecimal, AirMass.Predicate](_ => AirMassErrorMsg)
      .andThen(AirMass.Value.reverse)

  val AirMassConstraintValidate: ValidSplitEpi[String, AirMass, AirMassBound] =
    ValidSplitEpi
      .forRefined[String, AirMass, AirMassBound.Predicate](_ => AirMassErrorMsg)
      .andThen(AirMassBound.Value.reverse)

  val AirMassElevationRangeValidWedge: InputValidWedge[AirMassBound] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(AirMassValidate.toErrorsValidSplitEpiUnsafe)
      .andThen(AirMassConstraintValidate.toErrorsValidSplitEpiUnsafe)

  val HourAngleConstraintValidate: ValidSplitEpi[String, BigDecimal, HourAngleBound] =
    ValidSplitEpi
     .forRefined[String, BigDecimal, HourAngleBound.Predicate](_ => HourAngleErrorMsg)
    .andThen(HourAngleBound.Value.reverse)

  val HourAngleElevationRangeValidWedge: InputValidWedge[HourAngleBound] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(HourAngleConstraintValidate.toErrorsValidSplitEpiUnsafe)

  private val ImageQualityErrorMsg: String  = s"Must be more than 0.00 and up to 5.00"

  val ImageQualityValidWedge: InputValidWedge[ImageQuality] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 2.refined)
      .andThen(ImageQuality.FromArcSeconds, _ => ImageQualityErrorMsg.toEitherErrorsUnsafe)

  private val CloudExtinctionErrorMsg: String  = s"Must be 0.00 to 5.00"

  val CloudExtinctionValidWedge: InputValidWedge[CloudExtinction] =
    InputValidWedge.truncatedBigDecimal(decimals = 2.refined)
      .andThen(Extinction.FromVegaMagnitude, _ => CloudExtinctionErrorMsg.toEitherErrorsUnsafe)
       .andThen(CloudExtinction.From, _ => CloudExtinctionErrorMsg.toEitherErrorsUnsafe)
}

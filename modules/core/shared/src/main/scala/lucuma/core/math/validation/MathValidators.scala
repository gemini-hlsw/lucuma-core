// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.validation

import cats.data.NonEmptyChain
import eu.timepit.refined.auto._
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.math.truncated._
import lucuma.core.validation._

trait MathValidators {

  val epoch: ValidFormatInput[Epoch] =
    ValidFormatInput.fromPrism(Epoch.fromString, "Invalid epoch")

  // We can't have a general `ValidFormatInput[Angle]`.
  // It fails format roundtrip law check because of rounding.
  val truncatedAngleSignedDegrees: ValidFormatInput[TruncatedAngle] =
    ValidFormatInput(
      _.toBigDecimalOption
        .map(Angle.fromBigDecimalDegrees)
        .map(TruncatedAngle.apply)
        .toRight("Invalid Angle")
        .toEitherInputUnsafe,
      ta => f"${ta.angle.toSignedBigDecimalDegrees}%.2f"
    )

  val rightAscension: ValidFormatInput[RightAscension] =
    ValidFormatInput(
      s =>
        RightAscension.fromStringHMS
          .getOption(s)
          .toRight("Invalid Right Ascension")
          .toEitherInputUnsafe,
      RightAscension.fromStringHMS.reverseGet
    )

  val truncatedRA: ValidFormatInput[TruncatedRA] =
    ValidFormatInput(
      rightAscension
        .andThen(TruncatedRA.rightAscension, NonEmptyChain("Invalid Right Ascension"))
        .getValid,
      tra => {
        val s = RightAscension.fromStringHMS.reverseGet(tra.ra)
        s.dropRight(3)
      }
    )

  val declination: ValidFormatInput[Declination] =
    ValidFormatInput(
      s =>
        Declination.fromStringSignedDMS
          .getOption(s)
          .toRight("Invalid Declination")
          .toEitherInputUnsafe,
      Declination.fromStringSignedDMS.reverseGet
    )

  val truncatedDec: ValidFormatInput[TruncatedDec] =
    ValidFormatInput(
      declination
        .andThen(TruncatedDec.declination, NonEmptyChain("Invalid Declination"))
        .getValid,
      tdec => { // Move this as toString/format in the TruncatedDec object ???
        val s = Declination.fromStringSignedDMS.reverseGet(tdec.dec)
        s.dropRight(4)
      }
    )
}

object MathValidators extends MathValidators

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.validation

import eu.timepit.refined.auto._
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.truncated._
import lucuma.core.validation._
import cats.data.NonEmptyChain
import lucuma.core.math.Angle
import lucuma.core.math.Epoch

trait MathValidators {

  // TODO TEST ALL THESE!!!!!

  val epoch: ValidFormatInput[Epoch] =
    ValidFormatInput.fromPrism(Epoch.fromString, "Invalid epoch")

  val angle: ValidFormatInput[Angle] =
    ValidFormatInput(
      _.toDoubleOption
        .map(Angle.fromDoubleArcseconds)
        .toRight("Invalid Angle")
        .toEitherInputUnsafe,
      a => (a.toMicroarcseconds / 1000000.0).toString
    )

  val truncatedAngle: ValidFormatInput[TruncatedAngle] =
    ValidFormatInput(
      angle.andThen(TruncatedAngle.angle, NonEmptyChain("Invalid Angle")).getValid,
      ta => f"${ta.angle.toSignedDoubleDegrees}%.2f"
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
      tdec => {
        val s = Declination.fromStringSignedDMS.reverseGet(tdec.dec)
        s.dropRight(4)
      }
    )
}

object MathValidators extends MathValidators

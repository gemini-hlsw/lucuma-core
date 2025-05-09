// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.validation

import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.syntax.string.*
import lucuma.core.syntax.validation.*
import lucuma.core.validation.*
import lucuma.refined.*
import spire.math.Rational

trait MathValidators {

  val epoch: InputValidSplitEpi[Epoch] =
    InputValidSplitEpi.fromPrism(Epoch.fromString, "Invalid epoch".refined)

  val epochNoScheme:InputValidWedge[Epoch] =
    InputValidWedge.fromFormat(Epoch.fromStringNoScheme, "Invalid epoch".refined)

  val angleArcSec: InputValidSplitEpi[Angle] =
    InputValidSplitEpi(
      _.parseBigDecimalOption
        .map(Angle.fromBigDecimalArcseconds)
        .toRight("Invalid Angle")
        .toEitherErrorsUnsafe,
      a => (a.toMicroarcseconds / 1000000.0).toString
    )

  private def truncateAngleSignedArcSec(angle: Angle): Angle =
    Angle.signedDecimalArcseconds.reverseGet(
      (Angle.signedDecimalArcseconds.get(angle) * 100)
        .setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP) / 100 + 0.0
    )
  
  val truncatedAngleSignedArcSec: InputValidWedge[Angle] =
    InputValidWedge(
      _.parseBigDecimalOption.map(Angle.signedDecimalArcseconds.reverseGet).map(truncateAngleSignedArcSec)
      .toRight("Invalid Angle")
      .toEitherErrorsUnsafe,
      a => f"${Angle.signedDecimalArcseconds.get(truncateAngleSignedArcSec(a))}%.2f".replace("-0.00", "0.00")
    )

  private def truncateDegrees(angle: Angle): Angle =
    Angle.fromBigDecimalDegrees(
      (angle.toBigDecimalDegrees * 100)
        .setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP) / 100 + 0.0
    )

  val truncatedAngleDegrees: InputValidWedge[Angle] =
    InputValidWedge(
      _.parseBigDecimalOption
        .map(Angle.fromBigDecimalDegrees)
        .map(truncateDegrees)
        .toRight("Invalid Angle")
        .toEitherErrorsUnsafe,
      a => f"${truncateDegrees(a).toBigDecimalDegrees}%.2f".replace("360.00", "0.00")
    )

  val truncatedAngleSignedDegrees: InputValidWedge[Angle] =
    InputValidWedge(
      truncatedAngleDegrees.getValid,
      a =>
        f"${truncateDegrees(a).toSignedBigDecimalDegrees}%.2f"
          .replace("-0.00", "0.00")
          .replaceAll("^180.00", "-180.00")
    )

  val rightAscension: InputValidSplitEpi[RightAscension] =
    InputValidSplitEpi(
      s =>
        RightAscension.fromStringHMS
          .getOption(s)
          .toRight("Invalid Right Ascension")
          .toEitherErrorsUnsafe,
      RightAscension.fromStringHMS.reverseGet
    )

  private def truncateRA(ra: RightAscension): RightAscension = {
    val microSecs = Rational(ra.toHourAngle.toMicroseconds, 1000).round.toLong * 1000
    RightAscension(HourAngle.fromMicroseconds(microSecs))
  }

  val truncatedRA: InputValidWedge[RightAscension] =
    InputValidWedge(
      rightAscension.getValid.andThen(_.map(truncateRA)),
      ra => RightAscension.fromStringHMS.reverseGet(truncateRA(ra)).dropRight(3)
    )

  val declination: InputValidSplitEpi[Declination] =
    InputValidSplitEpi(
      s =>
        Declination.fromStringSignedDMS
          .getOption(s)
          .toRight("Invalid Declination")
          .toEitherErrorsUnsafe,
      Declination.fromStringSignedDMS.reverseGet
    )

  private def truncateDec(dec: Declination): Declination = {
    val microArcSecs = Rational(dec.toAngle.toMicroarcseconds, 10000).round.toLong * 10000
    Declination.fromAngleWithCarry(Angle.fromMicroarcseconds(microArcSecs))._1
  }

  val truncatedDec: InputValidWedge[Declination] =
    InputValidWedge(
      declination.getValid.andThen(_.map(truncateDec)),
      dec =>
        Declination.fromStringSignedDMS
          .reverseGet(truncateDec(dec))
          .dropRight(4)
          .replace("-00:00:00.00", "+00:00:00.00")
    )
}

object MathValidators extends MathValidators

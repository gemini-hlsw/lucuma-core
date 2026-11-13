// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.validation

import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.refined.auto.*
import lucuma.core.syntax.string.*
import lucuma.core.syntax.validation.*
import lucuma.core.validation.*
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

  private def roundAngleSignedArcSec(angle: Angle): Angle =
    Angle.signedDecimalArcseconds.reverseGet(
      (Angle.signedDecimalArcseconds.get(angle) * 100)
        .setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP) / 100 + 0.0
    )

  val roundedAngleSignedArcSec: InputValidWedge[Angle] =
    InputValidWedge(
      _.parseBigDecimalOption.map(Angle.signedDecimalArcseconds.reverseGet).map(roundAngleSignedArcSec)
      .toRight("Invalid Angle")
      .toEitherErrorsUnsafe,
      a => f"${Angle.signedDecimalArcseconds.get(roundAngleSignedArcSec(a))}%.2f".replace("-0.00", "0.00")
    )

  private def roundDegrees(angle: Angle): Angle =
    Angle.fromBigDecimalDegrees(
      (angle.toBigDecimalDegrees * 100)
        .setScale(0, scala.math.BigDecimal.RoundingMode.HALF_UP) / 100 + 0.0
    )

  val roundedAngleDegrees: InputValidWedge[Angle] =
    InputValidWedge(
      _.parseBigDecimalOption
        .map(Angle.fromBigDecimalDegrees)
        .map(roundDegrees)
        .toRight("Invalid Angle")
        .toEitherErrorsUnsafe,
      a => f"${roundDegrees(a).toBigDecimalDegrees}%.2f".replace("360.00", "0.00")
    )

  val roundedAngleSignedDegrees: InputValidWedge[Angle] =
    InputValidWedge(
      roundedAngleDegrees.getValid,
      a =>
        f"${roundDegrees(a).toSignedBigDecimalDegrees}%.2f"
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

  private def roundRA(ra: RightAscension): RightAscension = {
    val microSecs = Rational(ra.toHourAngle.toMicroseconds, 1000).round.toLong * 1000
    RightAscension(HourAngle.fromMicroseconds(microSecs))
  }

  val roundedRA: InputValidWedge[RightAscension] =
    InputValidWedge(
      rightAscension.getValid.andThen(_.map(roundRA)),
      ra => RightAscension.fromStringHMS.reverseGet(roundRA(ra)).dropRight(3)
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

  private def roundDec(dec: Declination): Declination = {
    val microArcSecs = Rational(dec.toAngle.toMicroarcseconds, 10000).round.toLong * 10000
    Declination.fromAngleWithCarry(Angle.fromMicroarcseconds(microArcSecs))._1
  }

  val roundedDec: InputValidWedge[Declination] =
    InputValidWedge(
      declination.getValid.andThen(_.map(roundDec)),
      dec =>
        Declination.fromStringSignedDMS
          .reverseGet(roundDec(dec))
          .dropRight(4)
          .replace("-00:00:00.00", "+00:00:00.00")
    )

  @deprecated("Renamed to roundedAngleSignedArcSec, it rounds rather than truncates", "0.278.0")
  val truncatedAngleSignedArcSec: InputValidWedge[Angle] = roundedAngleSignedArcSec

  @deprecated("Renamed to roundedAngleDegrees, it rounds rather than truncates", "0.278.0")
  val truncatedAngleDegrees: InputValidWedge[Angle] = roundedAngleDegrees

  @deprecated("Renamed to roundedAngleSignedDegrees, it rounds rather than truncates", "0.278.0")
  val truncatedAngleSignedDegrees: InputValidWedge[Angle] = roundedAngleSignedDegrees

  @deprecated("Renamed to roundedRA, it rounds rather than truncates", "0.278.0")
  val truncatedRA: InputValidWedge[RightAscension] = roundedRA

  @deprecated("Renamed to roundedDec, it rounds rather than truncates", "0.278.0")
  val truncatedDec: InputValidWedge[Declination] = roundedDec
}

object MathValidators extends MathValidators

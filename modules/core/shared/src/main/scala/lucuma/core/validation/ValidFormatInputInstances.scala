// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.truncated._
import singleton.ops._

/**
 * Convenience ValidFormatInput instances.
 */
trait ValidFormatInputInstances {
  val nonEmptyValidFormat = ValidFormatInput[NonEmptyString](
    s => NonEmptyString.from(s).toValidInputUnsafe,
    _.toString
  )

  def intValidFormat(errorMessage: NonEmptyString = "Must be an integer") =
    ValidFormatInput[Int](
      s => fixIntString(s).toIntOption.toRight(errorMessage).toValidInput,
      _.toString
    )

  // Does not, and cannot, format to a particular number of decimal places. For that
  // you need a TruncatedBigDecimal.
  def bigDecimalValidFormat(errorMessage: NonEmptyString = "Must be a number") =
    ValidFormatInput[BigDecimal](
      s => fixDecimalString(s).toBigDecimalOption.toRight(errorMessage).toValidInput,
      _.toString.toLowerCase.replace("+", "") // Strip + sign from exponent.
    )

  def truncatedBigDecimalValidFormat[Dec <: XInt](
    errorMessage: NonEmptyString = "Must be a number"
  )(implicit req: Require[&&[Dec > 0, Dec < 10]], vo: ValueOf[Dec]) =
    ValidFormatInput[TruncatedBigDecimal[Dec]](
      s => bigDecimalValidFormat(errorMessage).getValid(s).map(TruncatedBigDecimal(_)),
      tbd => s"%.${vo.value}f".format(tbd.value)
    )

  val truncatedRA = ValidFormatInput[TruncatedRA](
    s =>
      RightAscension.fromStringHMS
        .getOption(s)
        .map(TruncatedRA(_))
        .toRight("Invalid Right Ascension")
        .toValidInputUnsafe,
    tra => {
      val s = RightAscension.fromStringHMS.reverseGet(tra.ra)
      s.dropRight(3)
    }
  )

  val truncatedDec = ValidFormatInput[TruncatedDec](
    s =>
      Declination.fromStringSignedDMS
        .getOption(s)
        .map(TruncatedDec(_))
        .toRight("Invalid Declination")
        .toValidInputUnsafe,
    tdec => {
      val s = Declination.fromStringSignedDMS.reverseGet(tdec.dec)
      s.dropRight(4)
    }
  )

  private def fixIntString(str: String): String = str match {
    case ""  => "0"
    case "-" => "-0"
    case _   => str
  }

  protected def fixDecimalString(str: String): String = str match {
    case ""  => "0"
    case "-" => "-0"
    case "." => "0."
    case _   => str
  }
}

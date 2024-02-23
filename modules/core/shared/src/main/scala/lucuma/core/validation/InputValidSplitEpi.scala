// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.{Validate => RefinedValidate}
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics.*
import lucuma.core.syntax.string.*
import lucuma.refined.*
import monocle.Iso
import monocle.Prism

import java.math.RoundingMode
import java.text.DecimalFormat
import scala.util.Try

/**
 * Convenience version of `ValidSplitEpi` when the error type is `NonEmptyChain[NonEmptyString]` and
 * `T` is `String`.
 */
object InputValidSplitEpi {

  /**
   * Build a `InputValidSplitEpi` that's always valid and doesn't normalize or format
   */
  val id: InputValidSplitEpi[String] = ValidSplitEpi.id

  /**
   * Build a `InputValidSplitEpi` from `getValid` and `reverseGet` functions
   */
  def apply[A](
    getValid:   String => EitherErrors[A],
    reverseGet: A => String
  ): InputValidSplitEpi[A] =
    ValidSplitEpi(getValid, reverseGet)

  /**
   * Build a `InputValidSplitEpi` from a `Format`
   */
  def fromFormat[A](
    format:       Format[String, A],
    errorMessage: NonEmptyString = "Invalid format".refined
  ): InputValidSplitEpi[A] =
    ValidSplitEpi(
      format.getOption.andThen(_.toRight(errorMessage).toEitherErrors),
      format.reverseGet
    )

  /**
   * Build a `InputValidSplitEpi` from a `Prism`
   */
  def fromPrism[A](
    prism:        Prism[String, A],
    errorMessage: NonEmptyString = "Invalid value".refined
  ): InputValidSplitEpi[A] =
    fromFormat(Format.fromPrism(prism), errorMessage)

  /**
   * Build a `InputValidSplitEpi` from an `Iso`
   */
  def fromIso[A](iso: Iso[String, A]): InputValidSplitEpi[A] =
    ValidSplitEpi(
      (iso.get).andThen(_.asRight),
      iso.reverseGet
    )

  /**
   * Build a `InputValidSplitEpi` for `String Refined P`
   */
  def refinedString[P](implicit
    v: RefinedValidate[String, P]
  ): InputValidSplitEpi[String Refined P] =
    id.refined[P](_ => NonEmptyChain("Invalid value".refined))

  /**
   * `InputValidSplitEpi` for `NonEmptyString`
   */
  val nonEmptyString: InputValidSplitEpi[NonEmptyString] =
    refinedString[NonEmpty].withErrorMessage(_ => "Must be defined".refined)

  /**
   * `InputValidSplitEpi` for `Int`
   */
  val int: InputValidSplitEpi[Int] =
    InputValidSplitEpi(
      s => fixIntString(s).toIntOption.toRight(NonEmptyChain("Must be an integer".refined)),
      _.toString
    )

  /**
   * Build a `InputValidSplitEpi` for `Int Refined P`
   */
  def refinedInt[P](implicit v: RefinedValidate[Int, P]): InputValidSplitEpi[Int Refined P] =
    int.refined[P](_ => NonEmptyChain("Invalid format".refined))

  /**
   * `InputValidSplitEpi` for `PosInt`
   */
  val posInt: InputValidSplitEpi[PosInt] =
    refinedInt[Positive]

  /**
   * `InputValidSplitEpi` for `BigDecimal`.
   *
   * Does not, and cannot, format to a particular number of decimal places. For that you need a
   * `TruncatedBigDecimal`.
   */
  val bigDecimal: InputValidSplitEpi[BigDecimal] =
    InputValidSplitEpi(
      s => fixDecimalString(s).parseBigDecimalOption.toRight(NonEmptyChain("Must be a number".refined)),
      _.toString.toLowerCase.replace("+", "") // Strip + sign from exponent.
    )

  /**
   * Build a `InputValidSplitEpi` for `BigDecimal Refined P`
   */
  def refinedBigDecimal[P](implicit
    v: RefinedValidate[BigDecimal, P]
  ): InputValidSplitEpi[BigDecimal Refined P] =
    bigDecimal.refined[P](_ => NonEmptyChain("Invalid format".refined))

  /**
   * `InputValidSplitEpi` for `PosBigDecimal`
   */
  val posBigDecimal: InputValidSplitEpi[PosBigDecimal] =
    refinedBigDecimal[Positive]

  /**
   * `InputValidSplitEpi` for `BigDecimal`, formatting with only one integer digit.
   */
  val bigDecimalWithScientificNotation: InputValidSplitEpi[BigDecimal] =
    InputValidSplitEpi(
      bigDecimal.getValid,
      n => Try(scientificNotationFormat(n)).toOption.orEmpty
    ) // We shouldn't need to catch errors here, but https://github.com/scala-js/scala-js/issues/4655

  /**
   * Build a `InputValidSplitEpi` for `BigDecimal Refined P`, formatting with only one integer
   * digit.
   */
  def refinedBigDecimalWithScientificNotation[P](implicit
    v: RefinedValidate[BigDecimal, P]
  ): InputValidSplitEpi[BigDecimal Refined P] =
    bigDecimalWithScientificNotation.refined[P](_ => NonEmptyChain("Invalid format".refined))

  /**
   * `InputValidSplitEpi` for `PosBigDecimal`, formatting with only one integer digit.
   */
  val posBigDecimalWithScientificNotation: InputValidSplitEpi[PosBigDecimal] =
    refinedBigDecimalWithScientificNotation[Positive]

  private def fixIntString(str: String): String = str match {
    case ""  => "0"
    case "-" => "-0"
    case _   => str
  }

  private def fixDecimalString(str: String): String = str match {
    case ""  => "0"
    case "-" => "-0"
    case "." => "0."
    case _   => str
  }

  private def scientificNotationFormat(
    x:        BigDecimal,
    decimals: Option[DigitCount] = none
  ): String = {
    val formatter = new DecimalFormat("0.0E0")
    formatter.setRoundingMode(RoundingMode.HALF_UP)
    formatter.setMaximumFractionDigits(decimals.map(_.value).getOrElse(x.precision - 1))
    formatter.format(x.bigDecimal).stripSuffix("E0").toLowerCase
  }
}

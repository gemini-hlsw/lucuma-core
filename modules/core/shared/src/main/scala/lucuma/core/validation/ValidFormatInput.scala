// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.{Validate => RefinedValidate}
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.truncated._
import lucuma.core.optics._
import monocle.Iso
import monocle.Prism
import singleton.ops._

import java.math.RoundingMode
import java.text.DecimalFormat
import scala.util.Try

/**
 * Convenience version of `ValidFormat` when the error type is `NonEmptyChain[NonEmptyString]` and
 * `T` is `String`.
 */
object ValidFormatInput {

  // TODO TEST ALL THESE!!!!!

  /**
   * Build a `ValidFormatInput` that's always valid and doesn't normalize or format
   */
  val id: ValidFormatInput[String] = ValidFormat.id

  /**
   * Build a `ValidFormatInput` from `getValid` and `reverseGet` functions
   */
  def apply[A](
    getValid:   String => EitherInput[A],
    reverseGet: A => String
  ): ValidFormatInput[A] =
    ValidFormat(getValid, reverseGet)

  /**
   * Build a `ValidFormatInput` from a `Format`
   */
  def fromFormat[A](
    format:       Format[String, A],
    errorMessage: NonEmptyString = "Invalid format"
  ): ValidFormatInput[A] =
    ValidFormat(
      format.getOption.andThen(_.toRight(errorMessage).toEitherInput),
      format.reverseGet
    )

  /**
   * Build a `ValidFormatInput` from a `Prism`
   */
  def fromPrism[A](
    prism:        Prism[String, A],
    errorMessage: NonEmptyString = "Invalid value"
  ): ValidFormatInput[A] =
    fromFormat(Format.fromPrism(prism), errorMessage)

  /**
   * Build a `ValidFormatInput` from an `Iso`
   */
  def fromIso[A](iso: Iso[String, A]): ValidFormatInput[A] =
    ValidFormat(
      (iso.get _).andThen(_.asRight),
      iso.reverseGet
    )

  /**
   * Build a `ValidFormatInput` for `String Refined P`
   */
  def refinedString[P](implicit v: RefinedValidate[String, P]): ValidFormatInput[String Refined P] =
    id.refined[P](NonEmptyChain("Invalid value"))

  /**
   * `ValidFormatInput` for `NonEmptyString`
   */
  val nonEmptyString: ValidFormatInput[NonEmptyString] =
    refinedString[NonEmpty].withErrorMessage("Must be defined")

  /**
   * `ValidFormatInput` for `Int`
   */
  val int: ValidFormatInput[Int] =
    ValidFormatInput(
      s => fixIntString(s).toIntOption.toRight(NonEmptyChain("Must be an integer")),
      _.toString
    )

  /**
   * Build a `ValidFormatInput` for `Int Refined P`
   */
  def refinedInt[P](implicit v: RefinedValidate[Int, P]): ValidFormatInput[Int Refined P] =
    int.refined[P](NonEmptyChain("Invalid format"))

  /**
   * `ValidFormatInput` for `PosInt`
   */
  val posInt: ValidFormatInput[PosInt] =
    refinedInt[Positive]

  /**
   * `ValidFormatInput` for `BigDecimal`.
   *
   * Does not, and cannot, format to a particular number of decimal places. For that you need a
   * `TruncatedBigDecimal`.
   */
  val bigDecimal: ValidFormatInput[BigDecimal] =
    ValidFormatInput(
      s => fixDecimalString(s).toBigDecimalOption.toRight(NonEmptyChain("Must be a number")),
      _.toString.toLowerCase.replace("+", "") // Strip + sign from exponent.
    )

  /**
   * Build a `ValidFormatInput` for `BigDecimal Refined P`
   */
  def refinedBigDecimal[P](implicit
    v: RefinedValidate[BigDecimal, P]
  ): ValidFormatInput[BigDecimal Refined P] =
    bigDecimal.refined[P](NonEmptyChain("Invalid format"))

  /**
   * `ValidFormatInput` for `PosBigDecimal`
   */
  val posBigDecimal: ValidFormatInput[PosBigDecimal] =
    refinedBigDecimal[Positive]

  /**
   * Build a `ValidFormatInput` for `TruncatedBigDecimal[Dec]`
   */
  def truncatedBigDecimal[Dec <: XInt](implicit
    req: Require[&&[Dec > 0, Dec < 10]],
    vo:  ValueOf[Dec]
  ): ValidFormatInput[TruncatedBigDecimal[Dec]] =
    ValidFormatInput(
      s => bigDecimal.getValid(s).map(TruncatedBigDecimal[Dec](_)),
      tbd => s"%.${vo.value}f".format(tbd.value)
    )

  /**
   * Build a `ValidFormatInput` for `TruncatedRefinedBigDecimal[P, Dec]`
   */
  def truncatedRefinedBigDecimal[P, Dec <: XInt](implicit
    v:   RefinedValidate[BigDecimal, P],
    req: Require[&&[Dec > 0, Dec < 10]],
    vo:  ValueOf[Dec]
  ): ValidFormatInput[TruncatedRefinedBigDecimal[P, Dec]] =
    ValidFormatInput(
      refinedBigDecimal[P]
        .andThen(
          TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[P, Dec],
          NonEmptyChain("Invalid format")
        )
        .getValid,
      trbd => s"%.${vo.value}f".format(trbd.value.value)
    )

  /**
   * `ValidFormatInput` for `BigDecimal`, formatting with only one integer digit.
   */
  val bigDecimalWithScientificNotation: ValidFormatInput[BigDecimal] =
    ValidFormatInput(
      bigDecimal.getValid,
      n => Try(scientificNotationFormat(n)).toOption.orEmpty
    ) // We shouldn't need to catch errors here, but https://github.com/scala-js/scala-js/issues/4655

  /**
   * Build a `ValidFormatInput` for `BigDecimal Refined P`, formatting with only one integer digit.
   */
  def refinedBigDecimalWithScientificNotation[P](implicit
    v: RefinedValidate[BigDecimal, P]
  ): ValidFormatInput[BigDecimal Refined P] =
    bigDecimalWithScientificNotation.refined[P](NonEmptyChain("Invalid format"))

  /**
   * `ValidFormatInput` for `PosBigDecimal`, formatting with only one integer digit.
   */
  val posBigDecimalWithScientificNotation: ValidFormatInput[PosBigDecimal] =
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

  private def scientificNotationFormat(x: BigDecimal, decimals: Option[Int] = none): String = {
    val formatter = new DecimalFormat("0.0E0")
    formatter.setRoundingMode(RoundingMode.HALF_UP)
    formatter.setMaximumFractionDigits(decimals.getOrElse(x.precision - 1))
    formatter.format(x.bigDecimal)
  }
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.{Validate => RefinedValidate}
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.collection.NonEmpty
import lucuma.core.math.truncated._
import lucuma.core.optics._
import monocle.Iso
import monocle.Prism
import singleton.ops._
import java.text.DecimalFormat
import java.math.RoundingMode

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
  def refinedString[P](
    error:      NonEmptyString = "Invalid format"
  )(implicit v: RefinedValidate[String, P]): ValidFormatInput[String Refined P] =
    id.refined[P](NonEmptyChain(error))

  /**
   * Build a `ValidFormatInput` for `NonEmptyString`
   */
  def nonEmptyString(
    error: NonEmptyString = "Must have a value"
  ): ValidFormatInput[NonEmptyString] =
    refinedString[NonEmpty](error)

  def int(errorMessage: NonEmptyString = "Must be an integer") =
    ValidFormatInput[Int](
      s => fixIntString(s).toIntOption.toRight(errorMessage).toEitherInput,
      _.toString
    )

  /**
   * Build a `ValidFormatInput` for `Int Refined P`
   */
  def refinedInt[P](
    error:      NonEmptyString = "Invalid format"
  )(implicit v: RefinedValidate[Int, P]): ValidFormatInput[Int Refined P] =
    int(error)
      .refined[P](NonEmptyChain(error))

  /**
   * Build a `ValidFormatInput` for `PosInt`
   */
  def posInt(
    error: NonEmptyString = "Invalid format"
  ): ValidFormatInput[PosInt] =
    refinedInt[Positive](error)

  /**
   * Build a `ValidFormatInput` for `BigDecimal`.
   *
   * Does not, and cannot, format to a particular number of decimal places. For that you need a
   * `TruncatedBigDecimal`.
   */
  def bigDecimal(errorMessage: NonEmptyString = "Must be a number") =
    ValidFormatInput[BigDecimal](
      s => fixDecimalString(s).toBigDecimalOption.toRight(errorMessage).toEitherInput,
      _.toString.toLowerCase.replace("+", "") // Strip + sign from exponent.
    )

  /**
   * Build a `ValidFormatInput` for `BigDecimal Refined P`
   */
  def refinedBigDecimal[P](
    error:      NonEmptyString = "Invalid format"
  )(implicit v: RefinedValidate[BigDecimal, P]): ValidFormatInput[BigDecimal Refined P] =
    bigDecimal(error)
      .refined[P](NonEmptyChain(error))

  /**
   * Build a `ValidFormatInput` for `PosBigDecimal`
   */
  def posBigDecimal(
    error: NonEmptyString = "Invalid format"
  ): ValidFormatInput[PosBigDecimal] =
    refinedBigDecimal[Positive](error)

  /**
   * Build a `ValidFormatInput` for `TruncatedBigDecimal[Dec]`
   */
  def truncatedBigDecimal[Dec <: XInt](
    errorMessage: NonEmptyString = "Must be a number"
  )(implicit req: Require[&&[Dec > 0, Dec < 10]], vo: ValueOf[Dec]) =
    ValidFormatInput[TruncatedBigDecimal[Dec]](
      s => bigDecimal(errorMessage).getValid(s).map(TruncatedBigDecimal(_)),
      tbd => s"%.${vo.value}f".format(tbd.value)
    )

  /**
   * Build a `ValidFormatInput` for `TruncatedRefinedBigDecimal[P, Dec]`
   */
  def truncatedRefinedBigDecimal[P, Dec <: XInt](
    error: NonEmptyString = "Invalid format"
  )(implicit
    v:     RefinedValidate[BigDecimal, P],
    req:   Require[&&[Dec > 0, Dec < 10]],
    vo:    ValueOf[Dec]
  ): ValidFormatInput[TruncatedRefinedBigDecimal[P, Dec]] =
    ValidFormatInput[TruncatedRefinedBigDecimal[P, Dec]](
      refinedBigDecimal[P](error)
        .andThen(TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[P, Dec], NonEmptyChain(error))
        .getValid,
      trbd => s"%.${vo.value}f".format(trbd.value.value)
    )

  /**
   * Build a `ValidFormatInput` for `BigDecimal` accepting scientific notation
   */
  def bigDecimalWithScientificNotation(
    error: NonEmptyString = "Invalid format"
  ): ValidFormatInput[BigDecimal] =
    ValidFormatInput(
      bigDecimal(error).getValid,
      n => Try(scientificNotationFormat(n)).toOption.orEmpty
    ) // We shouldn't need to catch errors here, but https://github.com/scala-js/scala-js/issues/4655

  /**
   * Build a `ValidFormatInput` for `BigDecimal Refined P` accepting scientific notation
   */
  def refinedBigDecimalWithScientificNotation[P](
    error:      NonEmptyString = "Invalid format"
  )(implicit v: RefinedValidate[BigDecimal, P]): ValidFormatInput[BigDecimal Refined P] =
    bigDecimalWithScientificNotation(error).refined[P](NonEmptyChain(error))

  /**
   * Build a `ValidFormatInput` for `PosBigDecimal` accepting scientific notation
   */
  def posBigDecimalWithScientificNotation(
    error: NonEmptyString = "Invalid format"
  ): ValidFormatInput[PosBigDecimal] =
    refinedBigDecimalWithScientificNotation[Positive](error)

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

  private def scientificNotationFormat(x: BigDecimal): String = {
    val formatter = new DecimalFormat("0.0E0")
    formatter.setRoundingMode(RoundingMode.HALF_UP)
    formatter.setMinimumFractionDigits(if (x.scale > 0) x.precision - 1 else x.scale)
    formatter.format(x.bigDecimal).stripSuffix("E0").toLowerCase
  }
}

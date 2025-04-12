// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.optics.*
import lucuma.core.syntax.validation.*
import lucuma.refined.*
import monocle.Iso
import monocle.Prism

import scala.math.BigDecimal.RoundingMode

/**
 * Convenience version of `ValidWedge` when the error type is `NonEmptyChain[NonEmptyString]` and
 * `T` is `String`.
 */
object InputValidWedge {

  /**
   * Build an `InputValidWedge` that's always valid and doesn't normalize or format
   */
  val id: InputValidWedge[String] = ValidWedge.id

  /**
   * Build an `InputValidWedge` from `getValid` and `reverseGet` functions
   */
  def apply[A](
    getValid:   String => EitherErrors[A],
    reverseGet: A => String
  ): InputValidWedge[A] =
    ValidWedge(getValid, reverseGet)

  /**
   * Build an `InputValidWedge` from a `Format`
   */
  def fromFormat[A](
    format:       Format[String, A],
    errorMessage: NonEmptyString = "Invalid format".refined
  ): InputValidWedge[A] =
    ValidWedge(
      format.getOption.andThen(_.toRight(errorMessage).toEitherErrors),
      format.reverseGet
    )

  /**
   * Build an `InputValidWedge` from a `Prism`
   */
  def fromPrism[A](
    prism:        Prism[String, A],
    errorMessage: NonEmptyString = "Invalid value".refined
  ): InputValidWedge[A] =
    fromFormat(Format.fromPrism(prism), errorMessage)

  /**
   * Build an `InputValidWedge` from an `Iso`
   */
  def fromIso[A](iso: Iso[String, A]): InputValidWedge[A] =
    ValidWedge(
      (iso.get).andThen(_.asRight),
      iso.reverseGet
    )

  /**
   * Build a `InputValidWedge` for `BigDecimal` truncated to `Dec` decimals.
   */
  def truncatedBigDecimal(decimals: DigitCount): InputValidWedge[BigDecimal] =
    InputValidWedge(
      InputValidSplitEpi.bigDecimal.getValid
        .andThen(_.map(_.setScale(decimals.value, RoundingMode.HALF_UP))),
        _.setScale(decimals.value, RoundingMode.HALF_UP)
          .underlying
          .toPlainString
          .replaceAll("^-0\\.(0+)$", "0.$1") // Remove negative 0
    )

  // We can't build a generic truncatedRefinedBigDecimal. Formatting may cause the refinement to break.
  // For example: a truncated PosBigDecimal may turn a small value into a 0, which is no longer positive.

  /**
   * Build a `InputValidWedge` for `PosBigDecimal` truncated to `Dec` decimals.
   */
  def truncatedPosBigDecimal(decimals: DigitCount): InputValidWedge[PosBigDecimal] = {
    val base     = truncatedBigDecimal(decimals).andThen(
      ValidWedge.forRefined[NonEmptyChain[NonEmptyString], BigDecimal, Positive](
        _ => NonEmptyChain("Invalid format".refined)
      )
    )
    val minValue = "0." + "0" * (decimals.value - 1) + "1"

    InputValidWedge(
      base.getValid,
      base.reverseGet.andThen(_.replaceAll("^0\\.0+$", minValue))
    )
  }
}

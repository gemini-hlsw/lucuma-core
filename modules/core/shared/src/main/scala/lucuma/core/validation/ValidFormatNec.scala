// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.syntax.all._
import lucuma.core.optics.Format
import lucuma.core.optics.ValidFormat
import monocle.Iso
import monocle.Prism

/**
 * Convenience version of `ValidFormat` when the error type is `NonEmptyChain[E]`
 */
object ValidFormatNec extends ValidFormatNecInstances {

  /**
   * Build a `ValidFormatNec` that's always valid and doesn't normalize or format
   */
  def id[E, A]: ValidFormatNec[E, A, A] = ValidFormat.id

  /**
   * Build a `ValidFormatNec` from `getValid` and `reverseGet` functions
   */
  def apply[E, T, A](
    getValid:   T => EitherNec[E, A],
    reverseGet: A => T
  ): ValidFormatNec[E, T, A] =
    ValidFormat(getValid, reverseGet)

  /**
   * Build a `ValidFormatNec` from a `Format`
   */
  def fromFormat[E, T, A](
    format: Format[T, A],
    error:  E
  ): ValidFormatNec[E, T, A] =
    ValidFormat(
      format.getOption.andThen(o => o.toRight(NonEmptyChain(error))),
      format.reverseGet
    )

  /**
   * Build a `ValidFormatNec` from a `Prism`
   */
  def fromPrism[E, T, A](
    prism: Prism[T, A],
    error: E
  ): ValidFormatNec[E, T, A] =
    fromFormat(Format.fromPrism(prism), error)

  /**
   * Build a `ValidFormatNec` from an `Iso`
   */
  def fromIso[E, T, A](iso: Iso[T, A]): ValidFormatNec[E, T, A] =
    ValidFormat(
      (iso.get _).andThen(_.asRight),
      iso.reverseGet
    )

}

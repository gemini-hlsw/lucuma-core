// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import lucuma.core.optics.Format
import lucuma.core.optics.ValidSplitEpi
import monocle.Iso
import monocle.Prism

/**
 * Convenience version of `ValidSplitEpi` when the error type is `NonEmptyChain[E]`
 */
object ValidSplitEpiNec extends ValidSplitEpiNecInstances {

  /**
   * Build a `ValidSplitEpiNec` that's always valid and doesn't normalize or format
   */
  def id[E, A]: ValidSplitEpiNec[E, A, A] = ValidSplitEpi.id

  /**
   * Build a `ValidSplitEpiNec` from `getValid` and `reverseGet` functions
   */
  def apply[E, A, B](
    getValid:   A => EitherNec[E, B],
    reverseGet: B => A
  ): ValidSplitEpiNec[E, A, B] =
    ValidSplitEpi(getValid, reverseGet)

  /**
   * Build a `ValidSplitEpiNec` from a `Format`
   */
  def fromFormat[E, A, B](
    format: Format[A, B],
    error:  E
  ): ValidSplitEpiNec[E, A, B] =
    ValidSplitEpi(
      format.getOption.andThen(o => o.toRight(NonEmptyChain(error))),
      format.reverseGet
    )

  /**
   * Build a `ValidSplitEpiNec` from a `Prism`
   */
  def fromPrism[E, A, B](
    prism: Prism[A, B],
    error: E
  ): ValidSplitEpiNec[E, A, B] =
    fromFormat(Format.fromPrism(prism), error)

  /**
   * Build a `ValidSplitEpiNec` from an `Iso`
   */
  def fromIso[E, A, B](iso: Iso[A, B]): ValidSplitEpiNec[E, A, B] =
    ValidSplitEpi(
      (iso.get).andThen(_.asRight),
      iso.reverseGet
    )

}

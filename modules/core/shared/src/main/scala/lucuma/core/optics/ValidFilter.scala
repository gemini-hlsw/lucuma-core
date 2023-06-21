// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.Order
import cats.syntax.all.*
import monocle.Prism

/**
 * A validating one-way endomorphic optic. `getValid` wraps a function that validates a value
 * without modifying it. `reverseGet` is always `identity`.
 *
 * No laws are necessary for this optic.
 */
case class ValidFilter[E, A](filter: A => IsValid[E]) extends ValidFormat[E, A, A] with Serializable { self =>
  final val getValid: A => Either[E, A] =
    a => filter(a) match
      case IsValid.Valid => a.asRight
      case IsValid.Invalid(e) => e.asLeft

  final val reverseGet: A => A = identity

  /** Always return a single instance of `E` in case of an invalid `T`. */
  def withError(e: E): ValidFilter[E, A] =
    ValidFilter(filter.andThen(_.mapError(_ => e)))

  /** Build a `ValidSplitEpi` with the same functionality. */
  def asValidSplitEpi: ValidSplitEpi[E, A, A] =
    ValidSplitEpi(getValid, reverseGet)

  /** Build a `ValidWedge` with the same functionality. */
  def asValidWedge: ValidWedge[E, A, A] =
    ValidWedge(getValid, reverseGet)

  /** Build a `Format` with the same funcionality, discarding the `E` instances. */
  def toFormat: Format[A, A] =
    Format(getValid.andThen(_.toOption), reverseGet)

  /** Build a `Prism` with the same funcionality, discarding the `E` instances. */
  def toPrism: Prism[A, A] =
    Prism(getValid.andThen(_.toOption))(reverseGet)

  /** Compose with a `ValidSplitEpi`. */
  def andThen[B](f: ValidSplitEpi[E, A, B]): ValidSplitEpi[E, A, B] =
    asValidSplitEpi.andThen(f)
}

object ValidFilter:
  /**
   * Build optic from getValid and reverseGet functions.
   */
  def apply[E, A](filter: A => Boolean, error: A => E): ValidFilter[E, A] =
    ValidFilter[E, A](a => if(filter(a)) IsValid.Valid else IsValid.Invalid(error(a)))

  def apply[E]: Applied[E] = new Applied[E] {}

  trait Applied[E]:
    def gt[A: Order](bound: A, error: A => E): ValidFilter[E, A] =
      ValidFilter[E, A](_ > bound, error)

    def lt[A: Order](bound: A, error: A => E): ValidFilter[E, A] =
      ValidFilter[E, A](_ < bound, error)

    def gte[A: Order](bound: A, error: A => E): ValidFilter[E, A] =
      ValidFilter[E, A](_ >= bound, error)

    def lte[A: Order](bound: A, error: A => E): ValidFilter[E, A] =
      ValidFilter[E, A](_ <= bound, error)

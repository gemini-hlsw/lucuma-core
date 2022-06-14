// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.{Validate => RefinedValidate}
import lucuma.core.optics._
import monocle.Iso
import monocle.Prism

/**
 * A validating and normalizing optic. Behaves similarly to `Format`, but the getter returns an
 * `Either[E, A]` instead of an `Option[A]`.
 *
 * Laws are the same for `Format`, except that `coverage` allows no normalization to happen as long
 * as there are invalid inputs.
 *
 * Composition with `Format` or stronger optics (`Prism` and `Iso`) yields another `ValidFormat`,
 * and require providing an `E` instance for the invalid cases.
 */
abstract class ValidFormat[E, T, A] extends Serializable { self =>
  val getValid: T => Either[E, A]

  val reverseGet: A => T

  /**
   * getValid and reverseGet, yielding a normalized formatted value if valid. Subsequent
   * getValid/reverseGet cycles are idempotent.
   */
  def normalize(t: T): Either[E, T] =
    getValid(t).map(reverseGet)

  /** Like getValid, but throws IllegalArgumentException when Invalid. */
  def unsafeGet(t: T): A =
    getValid(t).getOrElse {
      throw new IllegalArgumentException(s"unsafeGet failed: $t")
    }

  /** Compose with another Validate. */
  def andThen[B](f: ValidFormat[E, A, B]): ValidFormat[E, T, B] =
    ValidFormat[E, T, B](
      getValid(_).fold(_.asLeft, f.getValid),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a Format. */
  def andThen[B](f: Format[A, B], error: E): ValidFormat[E, T, B] =
    andThen(ValidFormat.fromFormat(f, error))

  /** Compose with a Prism. */
  def andThen[B](f: Prism[A, B], error: E): ValidFormat[E, T, B] =
    andThen(ValidFormat.fromPrism(f, error))

  /** Compose with an Iso. */
  def andThen[B](f: Iso[A, B]): ValidFormat[E, T, B] =
    ValidFormat[E, T, B](
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a SplitEpi. */
  def andThen[B](f: SplitEpi[A, B], error: E): ValidFormat[E, T, B] =
    andThen(ValidFormat.fromFormat(f.asFormat, error))

  /** Compose with a SplitMono. */
  def andThen[B](f: SplitMono[A, B]): ValidFormat[E, T, B] =
    ValidFormat(
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a Wedge. */
  def andThen[B](f: Wedge[A, B]): ValidFormat[E, T, B] =
    ValidFormat(
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  // /** ValidFormat is an invariant functor over A. */
  def imapA[B](f: B => A, g: A => B): ValidFormat[E, T, B] =
    ValidFormat(getValid(_).map(g), f.andThen(reverseGet))

  // /** ValidFormat is an invariant functor over T. */
  def imapT[S](f: T => S, g: S => T): ValidFormat[E, S, A] =
    ValidFormat(g.andThen(getValid), reverseGet.andThen(f))
}

object ValidFormat {

  /**
   * Build optic that's always valid and doesn't normalize or format
   */
  def id[E, A]: ValidFormat[E, A, A] = fromIso(Iso.id[A])

  /**
   * Build optic from getValid and reverseGet functions.
   */
  def apply[E, T, A](
    _getValid:   T => Either[E, A],
    _reverseGet: A => T
  ): ValidFormat[E, T, A] =
    new ValidFormat[E, T, A] {
      val getValid: T => Either[E, A] = _getValid
      val reverseGet: A => T          = _reverseGet
    }

  /**
   * Build optic from a Format
   */
  def fromFormat[E, T, A](format: Format[T, A], error: E): ValidFormat[E, T, A] =
    ValidFormat(
      format.getOption.andThen(o => Either.fromOption(o, error)),
      format.reverseGet
    )

  /**
   * Build optic from a Prism
   */
  def fromPrism[E, T, A](prism: Prism[T, A], error: E): ValidFormat[E, T, A] =
    fromFormat(Format.fromPrism(prism), error)

  /**
   * Build optic from a Iso
   */
  def fromIso[E, T, A](iso: Iso[T, A]): ValidFormat[E, T, A] =
    ValidFormat(
      (iso.get _).andThen(_.asRight),
      iso.reverseGet
    )

  def forRefined[E, A, P](error: E)(implicit
    v:                           RefinedValidate[A, P]
  ): ValidFormat[E, A, A Refined P] =
    fromPrism(refinedPrism[A, P], error)
}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.kernel.Eq
import cats.kernel.Monoid
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.{Validate => RefinedValidate}
import lucuma.core.optics._
import monocle.Iso
import monocle.Prism

/**
 * A validating and normalizing optic. Behaves similarly to `Format`, but the getter (now called
 * `getValid`) returns an `Either[E, A]` instead of an `Option[A]`.
 *
 * Laws are the same for `Format`, except that `coverage` allows no normalization to happen as long
 * as there are invalid inputs.
 *
 * Composition with `Format` or stronger optics (`Prism` and `Iso`) yields another `ValidSplitEpi`,
 * and require providing an `E` instance for the invalid cases.
 */
abstract class ValidSplitEpi[E, A, B] extends ValidFormat[E, A, B] with Serializable { self =>
  val getValid: A => Either[E, B]

  val reverseGet: B => A

  /**
   * getValid and reverseGet, yielding a normalized formatted value if valid. Subsequent
   * getValid/reverseGet cycles are idempotent.
   */
  def normalize(a: A): Either[E, A] =
    getValid(a).map(reverseGet)

  /** Like getValid, but throws IllegalArgumentException when Invalid. */
  def unsafeGet(a: A): B =
    getValid(a).getOrElse {
      throw new IllegalArgumentException(s"unsafeGet failed: $a")
    }

  /** Always return a single instance of `E` in case of an invalid `T`. */
  def withError(e: E): ValidSplitEpi[E, A, B] =
    ValidSplitEpi(
      getValid.andThen(_.leftMap(_ => e)),
      reverseGet
    )

  /** Build a `Format` with the same funcionality, discarding the `E` instances. */
  def asFormat: Format[A, B] =
    Format(a => getValid(a).toOption, reverseGet)

  /** Build a `ValidWedge` with the same functionality. */
  def asValidWedge: ValidWedge[E, A, B] =
    ValidWedge(getValid, reverseGet)

  /** Compose with another `ValidSplitEpi`. */
  def andThen[C](f: ValidSplitEpi[E, B, C]): ValidSplitEpi[E, A, C] =
    ValidSplitEpi(
      getValid(_).fold(_.asLeft, f.getValid),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a `ValidSplitEpi`. */
  def andThen[C](f: ValidWedge[E, B, C]): ValidWedge[E, A, C] =
    asValidWedge.andThen(f)

  /** Compose with a `Format`. */
  def andThen[C](f: Format[B, C], error: E): ValidSplitEpi[E, A, C] =
    andThen(ValidSplitEpi.fromFormat(f, error))

  /** Compose with a `Prism`. */
  def andThen[C](f: Prism[B, C], error: E): ValidSplitEpi[E, A, C] =
    andThen(ValidSplitEpi.fromPrism(f, error))

  /** Compose with an `Iso`. */
  def andThen[C](f: Iso[B, C]): ValidSplitEpi[E, A, C] =
    ValidSplitEpi(
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a `SplitEpi`. */
  def andThen[C](f: SplitEpi[B, C], error: E): ValidSplitEpi[E, A, C] =
    andThen(ValidSplitEpi.fromFormat(f.asFormat, error))

  /** Compose with a `SplitMono`. */
  def andThen[C](f: SplitMono[B, C]): ValidWedge[E, A, C] =
    ValidWedge(
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a `Wedge`. */
  def andThen[C](f: Wedge[B, C]): ValidWedge[E, A, C] =
    ValidWedge(
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  /** `ValidSplitEpi` is an invariant functor over A. */
  def imapA[C](f: A => C, g: C => A): ValidSplitEpi[E, C, B] =
    ValidSplitEpi(g.andThen(getValid), reverseGet.andThen(f))

  /** `ValidSplitEpi` is an invariant functor over B. */
  def imapB[C](f: C => B, g: B => C): ValidSplitEpi[E, A, C] =
    ValidSplitEpi(getValid.andThen(_.map(g)), f.andThen(reverseGet))

  /**
   * Build `ValidSplitEpi` from another one, but allow empty values to become `None`.
   */
  def optional(implicit ev: Monoid[A], eq: Eq[A]): ValidSplitEpi[E, A, Option[B]] =
    ValidSplitEpi(
      (a: A) =>
        if (a.isEmpty)
          none.asRight
        else
          self.getValid(a).map(_.some),
      (b: Option[B]) => b.foldMap(self.reverseGet)
    )

  /**
   * Build `ValidSplitEpi` from another one, refining the return type with predicate `P`.
   */
  def refined[P](error: E)(implicit ev: RefinedValidate[B, P]): ValidSplitEpi[E, A, Refined[B, P]] =
    this.andThen(ValidSplitEpi.forRefined[E, B, P](error))
}

object ValidSplitEpi {

  /**
   * Build optic that's always valid and doesn't normalize or format
   */
  def id[E, A]: ValidSplitEpi[E, A, A] = fromIso(Iso.id[A])

  /**
   * Build optic from getValid and reverseGet functions.
   */
  def apply[E, A, B](
    _getValid:   A => Either[E, B],
    _reverseGet: B => A
  ): ValidSplitEpi[E, A, B] =
    new ValidSplitEpi[E, A, B] {
      val getValid: A => Either[E, B] = _getValid
      val reverseGet: B => A          = _reverseGet
    }

  /**
   * Build optic from a Format
   */
  def fromFormat[E, A, B](format: Format[A, B], error: E): ValidSplitEpi[E, A, B] =
    ValidSplitEpi(
      format.getOption.andThen(o => Either.fromOption(o, error)),
      format.reverseGet
    )

  /**
   * Build optic from a Prism
   */
  def fromPrism[E, A, B](prism: Prism[A, B], error: E): ValidSplitEpi[E, A, B] =
    fromFormat(Format.fromPrism(prism), error)

  /**
   * Build optic from a Iso
   */
  def fromIso[E, A, B](iso: Iso[A, B]): ValidSplitEpi[E, A, B] =
    ValidSplitEpi(
      (iso.get _).andThen(_.asRight),
      iso.reverseGet
    )

  /**
   * Build optic for a Refined predicate
   */
  def forRefined[E, A, P](error: E)(implicit
    v:                           RefinedValidate[A, P]
  ): ValidSplitEpi[E, A, A Refined P] =
    fromPrism(refinedPrism[A, P], error)
}

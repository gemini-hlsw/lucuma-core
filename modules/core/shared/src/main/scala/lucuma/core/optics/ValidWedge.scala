// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.kernel.Eq
import cats.kernel.Monoid
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate as RefinedValidate
import lucuma.core.optics.*
import monocle.Iso
import monocle.Prism

/**
 * A validating and normalizing optic. It is to a `Wedge` what `ValidSplitEpi` is to a `SplitEpi`.
 *
 * Composition with stronger optics yields another `ValidWedge`, and require providing an `E`
 * instance for the invalid cases (except when composing with `ValidSplitEpi`).
 */
abstract class ValidWedge[E, A, B] extends ValidFormat[E, A, B] with Serializable { self =>
  val getValid: A => Either[E, B]

  val reverseGet: B => A

  def getOption: A => Option[B] = getValid.andThen(_.toOption)

  /** Normalize A via a round-trip through B. */
  def normalizeValidA(a: A): Either[E, A] =
    getValid.andThen(_.map(reverseGet))(a)

  /** Normalize B via a round-trip through A. */
  def normalizeB(b: B): Either[E, B] =
    getValid.compose(reverseGet)(b)

  /** Override `E` in case of an invalid `T`. */
  def withError(error: A => E): ValidWedge[E, A, B] =
    ValidWedge(
      a => getValid(a).leftMap(_ => error(a)),
      reverseGet
    )

  /** Compose with another `ValidWedge`. */
  def andThen[C](f: ValidWedge[E, B, C]): ValidWedge[E, A, C] =
    ValidWedge(
      getValid(_).fold(_.asLeft, f.getValid),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a `ValidSplitEpi`. */
  def andThen[C](f: ValidSplitEpi[E, B, C]): ValidWedge[E, A, C] =
    andThen(f.asValidWedge)

  /** Compose with a `Format`. */
  def andThen[C](f: Format[B, C], error: B => E): ValidWedge[E, A, C] =
    andThen(ValidWedge.fromFormat(f, error))

  /** Compose with a `Prism`. */
  def andThen[C](f: Prism[B, C], error: B => E): ValidWedge[E, A, C] =
    andThen(ValidWedge.fromPrism(f, error))

  /** Compose with an `Iso`. */
  def andThen[C](f: Iso[B, C]): ValidWedge[E, A, C] =
    ValidWedge(
      getValid(_).map(f.get),
      reverseGet.compose(f.reverseGet)
    )

  /** Compose with a `SplitEpi`. */
  def andThen[C](f: SplitEpi[B, C], error: B => E): ValidWedge[E, A, C] =
    andThen(ValidWedge.fromFormat(f.asFormat, error))

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

  /** `ValidWedge` is an invariant functor over A. */
  def imapA[C](f: A => C, g: C => A): ValidWedge[E, C, B] =
    ValidWedge(g.andThen(getValid), reverseGet.andThen(f))

  /** `ValidWedge` is an invariant functor over B. */
  def imapB[C](f: C => B, g: B => C): ValidWedge[E, A, C] =
    ValidWedge(getValid.andThen(_.map(g)), f.andThen(reverseGet))

  /**
   * Build `ValidWedge` from another one, but allow empty values to become `None`.
   */
  def optional(implicit ev: Monoid[A], eq: Eq[A]): ValidWedge[E, A, Option[B]] =
    ValidWedge(
      (a: A) =>
        if (a.isEmpty)
          none.asRight
        else
          self.getValid(a).map(_.some),
      (b: Option[B]) => b.foldMap(self.reverseGet)
    )

  /** Build a `ValidWedge` for an `Option` that doesn't allow empty values */
  def nonEmpty[C](msg: E)(using B =:= Option[C], Monoid[A], Eq[A]): ValidWedge[E, A, B] =
    ValidWedge(
      (a: A) =>
        if (a.isEmpty)
          Left(msg)
        else
          self.getValid(a),
      self.reverseGet
    )
}

object ValidWedge {

  /**
   * Build `ValidWedge` that's always valid and doesn't normalize or format
   */
  def id[E, A]: ValidWedge[E, A, A] = fromIso(Iso.id[A])

  /**
   * Build `ValidWedge` from getValid and reverseGet functions.
   */
  def apply[E, A, B](
    _getValid:   A => Either[E, B],
    _reverseGet: B => A
  ): ValidWedge[E, A, B] =
    new ValidWedge[E, A, B] {
      val getValid: A => Either[E, B] = _getValid
      val reverseGet: B => A          = _reverseGet
    }

  /**
   * Build `ValidWedge` from a `Format`
   */
  def fromFormat[E, A, B](format: Format[A, B], error: A => E): ValidWedge[E, A, B] =
    ValidWedge(
      a => format.getOption(a).toRight(error(a)),
      format.reverseGet
    )

  /**
   * Build `ValidWedge` from a `Prism`
   */
  def fromPrism[E, A, B](prism: Prism[A, B], error: A => E): ValidWedge[E, A, B] =
    fromFormat(Format.fromPrism(prism), error)

  /**
   * Build `ValidWedge` from a `Iso`
   */
  def fromIso[E, A, B](iso: Iso[A, B]): ValidWedge[E, A, B] =
    ValidWedge(
      (iso.get).andThen(_.asRight),
      iso.reverseGet
    )

  /**
   * Build `ValidWedge` for a `Refined` predicate
   */
  def forRefined[E, A, P](error: A => E)(implicit
    v:                           RefinedValidate[A, P]
  ): ValidWedge[E, A, A Refined P] =
    fromPrism(refinedPrism[A, P], error)
}

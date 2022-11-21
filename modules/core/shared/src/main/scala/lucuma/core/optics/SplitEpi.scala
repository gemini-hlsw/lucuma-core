// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.Functor
import cats.arrow.Category
import monocle.Iso
import monocle.Prism

/**
 * A split epimorphism, which we can think of as a weaker `Iso[A, B]` where `B` is a ''smaller''
 * . type. So `reverseGet andThen get` remains an identity but `get andThen reverseGet` is merely
 * idempotent (i.e., it normalizes values in `A`). The following statements hold:
 *
 *  - `reverseGet` is a ''section'' of `get`,
 *  - `get` is a ''retraction'' of `reverseGet`,
 *  - `B` is a ''retract'' of `A`,
 *  - the pair `(get, reverseGet)` is a ''splitting'' of the idempotent `get andThen reverseGet`.
 *
 * @param get any function A => B.
 * @param reverseGet a section of `get` such that `reverseGet andThen get` is an identity.
 * @see [[https://ncatlab.org/nlab/show/split+epimorphism Split Epimorphism]] at nLab
 */
final case class SplitEpi[A, B](get: A => B, reverseGet: B => A) {

  /** Modify the target of the SplitEpi using a function. */
  def modify(f: B => B): A => A =
    a => reverseGet(f(get(a)))

  /** Modify the target of the SplitEpi using Functor. */
  def modifyF[F[_]: Functor](f: B => F[B])(a: A): F[A] =
    Functor[F].map(f(get(a)))(reverseGet)

  /** Swapping `get` and `reverseGet` yields a `SplitMono`. */
  def reverse: SplitMono[B, A] =
    SplitMono(reverseGet, get)

  /** Compose with another SplitEpi. */
  def andThen[C](f: SplitEpi[B, C]): SplitEpi[A, C] =
    SplitEpi(get.andThen(f.get), reverseGet.compose(f.reverseGet))

  /** Compose with another SplitEpi. */
  def andThen[C](f: SplitMono[B, C]): Wedge[A, C] =
    Wedge(get.andThen(f.get), reverseGet.compose(f.reverseGet))

  /** Compose with an Iso. */
  def andThen[C](f: Iso[B, C]): SplitEpi[A, C] =
    SplitEpi(get.andThen(f.get), reverseGet.compose(f.reverseGet))

  /** Compose with a Prism to get a Format. */
  def andThen[C](bc: Prism[B, C]): Format[A, C] =
    Format(a => bc.getOption(get(a)), reverseGet.compose(bc.reverseGet))

  /** View this SplitEpi as a Format. */
  def asFormat: Format[A, B] =
    Format(a => Some(get(a)), reverseGet)

  /** View this SplitEpi as a Wedge. */
  def asWedge: Wedge[A, B] =
    Wedge(get, reverseGet)

  /** SplitEpi is an invariant functor over A. */
  def imapA[C](f: C => B, g: B => C): SplitEpi[A, C] =
    SplitEpi(get.andThen(g), f.andThen(reverseGet))

  /** SplitEpi is an invariant functor over B. */
  def imapB[C](f: A => C, g: C => A): SplitEpi[C, B] =
    SplitEpi(g.andThen(get), reverseGet.andThen(f))

  /**
   * get and reverseGet, yielding a normalized formatted value. Subsequent get/reverseGet cycles are
   * idempotent.
   */
  def normalize(a: A): A =
    reverseGet(get(a))

  /** If we can reverseGet a Product as a String we can implement a tagged toString like "Foo(stuff)". */
  def productToString(b: B)(implicit
    as:                  A =:= String,
    bp:                  B <:< Product
  ): String =
    taggedToString(b.productPrefix, b)

  /**
   * If we provide a tag like "Foo" and reverseGet as a String we can implement a nice toString like
   * "Foo(stuff)".
   */
  def taggedToString(tag: String, b: B)(implicit
    as:                   A =:= String
  ): String =
    new StringBuilder(tag)
      .append('(')
      .append(as(reverseGet(b)))
      .append(')')
      .toString

}

object SplitEpi {

  /** An Iso is trivially a SplitEpi. */
  def fromIso[A, B](p: Iso[A, B]): SplitEpi[A, B] =
    SplitEpi(p.get, p.reverseGet)

  /** SplitEpi forms a category. */
  implicit def SplitEpiCategory: Category[SplitEpi] =
    new Category[SplitEpi] {
      def id[A]: SplitEpi[A, A] = SplitEpi(identity, identity)
      def compose[A, B, C](f: SplitEpi[B, C], g: SplitEpi[A, B]): SplitEpi[A, C] = g.andThen(f)
    }

}

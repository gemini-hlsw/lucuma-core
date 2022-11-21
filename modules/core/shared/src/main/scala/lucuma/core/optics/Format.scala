// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import cats.arrow._
import monocle.Iso
import monocle.Prism

/**
  * A normalizing optic, isomorphic to Prism but with different laws, specifically `getOption`
  * need not be injective; i.e., distinct inputs may have the same getOption result, which combined
  * with a subsequent `reverseGet` yield a normalized form for A. Composition with stronger optics
  * (`Prism` and `Iso`) yields another `Format`.
  */
final case class Format[A, B](getOption: A => Option[B], reverseGet: B => A) {

  /** Like getOption, but throws IllegalArgumentException on failure. */
  def unsafeGet(a: A): B =
    getOption(a).getOrElse {
      throw new IllegalArgumentException(s"unsafeGet failed: $a")
    }

  /** Compose with another Format. */
  def andThen[C](f: Format[B, C]): Format[A, C] =
    Format(getOption(_).flatMap(f.getOption), reverseGet.compose(f.reverseGet))

  /** Compose with a Prism. */
  def andThen[C](f: Prism[B, C]): Format[A, C] =
    Format(getOption(_).flatMap(f.getOption), reverseGet.compose(f.reverseGet))

  /** Compose with an Iso. */
  def andThen[C](f: Iso[B, C]): Format[A, C] =
    Format(getOption(_).map(f.get), reverseGet.compose(f.reverseGet))

  /** Compose with a SplitEpi. */
  def composeSplitEpi[C](f: SplitEpi[B, C]): Format[A, C] =
    andThen(f.asFormat)

  /** Format is an invariant functor over B. */
  def imapB[C](f: C => B, g: B => C): Format[A, C] =
    Format(getOption(_).map(g), f.andThen(reverseGet))

  /** Format is an invariant functor over A. */
  def imapA[C](f: A => C, g: C => A): Format[C, B] =
    Format(g.andThen(getOption), reverseGet.andThen(f))

  /**
    * getOption and reverseGet, yielding a normalized formatted value. Subsequent getOption/reverseGet cycles are
    * idempotent.
    */
  def normalize(b: A): Option[A] =
    getOption(b).map(reverseGet)

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

object Format {

  /** A Prism is trivially a Format. */
  def fromPrism[A, B](p: Prism[A, B]): Format[A, B] =
    Format(p.getOption, p.reverseGet)

  /** An Iso is trivially a Format. */
  def fromIso[A, B](p: Iso[A, B]): Format[A, B] =
    Format(a => Some(p.get(a)), p.reverseGet)

  /** Format forms a category. */
  implicit def FormatCategory: Category[Format] =
    new Category[Format] {
      def id[A]: Format[A, A] = Format(Some(_), identity)
      def compose[A, B, C](f: Format[B, C], g: Format[A, B]): Format[A, C] = g.andThen(f)
    }

}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Contravariant
import cats.syntax.contravariant.*
import eu.timepit.refined.api.*

/**
 * Typeclass for things that can be shown in a user interface.
 * @group Typeclasses
 */
trait Display[A] {
  def shortName(a: A): String
  def longName(a: A): String = shortName(a)
}

object Display {
  def apply[A](using ev: Display[A]): ev.type = ev

  /**
   * Create an instance of `Display` using the provided functions.
   *
   * @param toShortName
   *   function that maps `A` to the shortName
   * @param toLongName
   *   function that maps `A` to the longName
   */
  def by[A](toShortName: A => String, toLongName: A => String): Display[A] =
    new Display[A] {
      def shortName(a: A)         = toShortName(a)
      override def longName(a: A) = toLongName(a)
    }

  /**
   * Create an instance of `Display` using the provided function for the shortName. The longName
   * will be the same as the shortName.
   *
   * @param toShortName
   *   function that maps A to the shortName
   */
  def byShortName[A](toShortName: A => String): Display[A] =
    new Display[A] {
      def shortName(a: A) = toShortName(a)
    }

  given Contravariant[Display] = new Contravariant[Display] {
    def contramap[A, B](fa: Display[A])(f: B => A): Display[B] =
      new Display[B] {
        def shortName(b: B)         = fa.shortName(f(b))
        override def longName(b: B) = fa.longName(f(b))
      }
  }

  given [T, P](using ev: Display[T]): Display[T Refined P] = ev.contramap(_.value)

  given Display[BigDecimal] = byShortName(_.toString.toLowerCase) // Use lower case "e" for exponent
}

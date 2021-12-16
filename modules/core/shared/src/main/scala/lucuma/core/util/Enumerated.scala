// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package util

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.all._
import lucuma.core.syntax.string._
import monocle.Prism

import io.circe._

/**
 * Typeclass for an enumerated type with unique string tags and a canonical ordering.
 * @group Typeclasses
 */
trait Enumerated[A] extends Order[A] with Encoder[A] with Decoder[A] {

  /** All members of this enumeration, in unspecified but canonical order. */
  def all: List[A]

  /** The tag for a given value. */
  def tag(a: A): String

  /** Select the member of this enumeration with the given tag, if any. */
  def fromTag(s: String): Option[A] = all.find(tag(_) === s)

  /** Select the member of this enumeration with the given tag, throwing if absent. */
  def unsafeFromTag(tag: String): A = fromTag(tag).getOrElse(sys.error("Invalid tag: " + tag))

  def compare(a: A, b: A): Int =
    Order[Int].compare(indexOfTag(tag(a)), indexOfTag(tag(b)))

  // Hashed index lookup, for efficient use as an `Order`.
  private lazy val indexOfTag: Map[String, Int] =
    all.zipWithIndex.iterator.map { case (a, n) => (tag(a), n) }.toMap

  // Decoder
  def apply(c: HCursor): Decoder.Result[A] =
    c.as[String].flatMap { s =>
      all
        .find(e => tag(e).toScreamingSnakeCase === s)
        .toRight(DecodingFailure(s"Could not parse enumerated type value '$s'", Nil))
    }

  // Encoder
  def apply(a: A): Json =
    Json.fromString(tag(a).toScreamingSnakeCase)

}

object Enumerated {

  def apply[A](implicit ev: Enumerated[A]): ev.type = ev

  def of[A <: Product](a: A, as: A*): Enumerated[A] =
    new Enumerated[A] {
      def all: List[A]      = a :: as.toList
      def tag(a: A): String = a.productPrefix
    }

  @inline
  def from[A](a: A, as: A*): Applied[A] = new Applied(a :: as.toList)
  def fromNEL[A](as: NonEmptyList[A]): Applied[A]   = new Applied(as.toList)

  class Applied[A] private[Enumerated] (val as: List[A]) extends AnyVal {
    def withTag(f: A => String): Enumerated[A] =
      new Enumerated[A] {
        def all: List[A]      = as
        def tag(a: A): String = f(a)
      }
  }

  def fromTag[A](implicit ev: Enumerated[A]): Prism[String, A] =
    Prism[String, A](ev.fromTag)(e => ev.tag(e))

}

/** @group Typeclasses */
trait Obsoletable[A] {
  def isActive(a: A): Boolean
  final def isObsolete(a: A): Boolean = !isActive(a)
}

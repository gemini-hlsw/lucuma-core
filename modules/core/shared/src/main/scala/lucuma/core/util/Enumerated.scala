// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package util

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.*
import lucuma.core.syntax.string.*
import monocle.Prism

import scala.quoted.*

/**
 * Typeclass for an enumerated type with unique string tags and a canonical ordering.
 * @group Typeclasses
 */
trait Enumerated[A] extends Order[A] with Encoder[A] with Decoder[A]:

  /** All members of this enumeration, in unspecified but canonical order. */
  def all: List[A]

  /** The tag for a given value. */
  def tag(a: A): String

  /** Select the member of this enumeration with the given tag, if any. */
  def fromTag(s: String): Option[A] = tagToValue.get(s).map(_._1)

  /** Select the member of this enumeration with the given tag, throwing if absent. */
  def unsafeFromTag(tag: String): A = fromTag(tag).getOrElse(sys.error("Invalid tag: " + tag))

  def compare(a: A, b: A): Int =
    Order[Int].compare(tagToValue(tag(a))._2, tagToValue(tag(b))._2)

  // Hashed tag lookup to value and canonical index, for efficient use in `fromTag` and `Order`.
  private lazy val tagToValue: Map[String, (A, Int)] =
    all.mapWithIndex((a, n) => (tag(a), (a, n))).toMap

  // Hashed screaming snake case tag lookup, for efficient use as a `Decoder`.
  private lazy val screamingSnakeCaseTagToValue: Map[String, A] =
    all.map(a => (tag(a).toScreamingSnakeCase, a)).toMap

  // Decoder
  def apply(c: HCursor): Decoder.Result[A] =
    c.as[String].flatMap { s =>
      screamingSnakeCaseTagToValue
        .get(s)
        .toRight(DecodingFailure(s"Could not parse enumerated type value '$s'", c.history))
    }

  // Encoder
  def apply(a: A): Json =
    Json.fromString(tag(a).toScreamingSnakeCase)

object Enumerated:

  def apply[A](using ev: Enumerated[A]): ev.type = ev

  @inline
  def from[A](a: A, as: A*): Applied[A]           = new Applied(a :: as.toList)
  def fromNEL[A](as: NonEmptyList[A]): Applied[A] = new Applied(as.toList)

  class Applied[A] private[Enumerated] (private val as: List[A]) extends AnyVal {
    def withTag(f: A => String): Enumerated[A] =
      new Enumerated[A] {
        def all: List[A]      = as
        def tag(a: A): String = f(a)
      }
  }

  def fromTag[A](using ev: Enumerated[A]): Prism[String, A] =
    Prism[String, A](ev.fromTag)(e => ev.tag(e))

  private def enumValuesImpl[E: Type](using Quotes): Expr[Array[E]] =
    import quotes.reflect.*
    val companion = Ref(TypeRepr.of[E].typeSymbol.companionModule)
    Select.unique(companion, "values").asExprOf[Array[E]]

  private def tagImpl[E](x: Expr[E])(using Quotes): Expr[String] =
    import quotes.reflect.*
    Select.unique(x.asTerm, "tag").asExprOf[String]

  private def enumeratedImpl[E: Type](using Quotes): Expr[Enumerated[E]] =
    '{
      Enumerated
        .fromNEL(NonEmptyList.fromListUnsafe(${ enumValuesImpl[E] }.toList))
        .withTag(x => ${ tagImpl[E]('x) })
    }

  inline def derived[E]: Enumerated[E] = ${ enumeratedImpl[E] }

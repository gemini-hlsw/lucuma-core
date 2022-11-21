// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats._
import cats.implicits._
import cats.kernel.BoundedEnumerable
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.char.Letter
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosLong
import io.circe._
import io.circe.syntax._
import monocle.Iso
import monocle.Prism

import scala.util.matching.Regex

/**
 * A typeclass for Lucuma identifiers, which are of the form T-26fd21b3 where T is a constant,
 * type-specific tag and the remainder is a positive hex-encoded Long, with lowercase alpha digits
 * and no leading zeros (thus having a unique string representation).
 *
 * {{{
 * case class Foo(id: Foo.Id, ...)
 * object Foo {
 *   case class Id(value: PosLong)
 *   object Id {
 *     implicit val GidId: Gid[Id] =
 *       Gid.instance('f', _.value, apply)
 *   }
 * }
 * }}}
 *
 * Database tables can use such values as primary keys.
 *
 * {{{
 * CREATE DOMAIN foo_dom AS VARCHAR CHECK(VALUE ~ '^f-([1-9a-f][0-9a-f]*)$');
 * CREATE SEQUENCE foo_seq START WITH 256;
 * CREATE TABLE foo (
 *   foo_id  varchar NOT NULL PRIMARY KEY DEFAULT 'f-' || to_hex(nextval('foo_seq')),
 *   ...
 * )
 * }}}
 */
final class Gid[A](
  val tag:        Char Refined Letter,
  val isoPosLong: Iso[A, PosLong]
) extends BoundedEnumerable[A]
    with Order[A]
    with Show[A]
    with Encoder[A]
    with Decoder[A] {

  // We use this in a few places
  private final val TagString: String = tag.value.toString

  final val regexPattern: String = raw"$TagString-([1-9a-f][0-9a-f]*)"

  private final val R: Regex = regexPattern.r

  /** Gids have a unique String representation. */
  final val fromString: Prism[String, A] = {

    def parse(s: String): Option[A] =
      s match {
        case R(s) =>
          for {
            signed <-
              Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(s, 16)).toOption
            pos    <- refineV[Positive](signed).toOption
          } yield isoPosLong.reverseGet(pos)
        case _    => None
      }

    def show(a: A): String =
      s"$TagString-${isoPosLong.get(a).value.toHexString}"

    Prism(parse)(show)
  }

  final val fromLong: Prism[Long, A] =
    Prism[Long, A](l => PosLong.from(l).toOption.map(isoPosLong.reverseGet))(a =>
      isoPosLong.get(a).value
    )

  // BoundedEnumerable

  final override def maxBound: A =
    isoPosLong.reverseGet(PosLong.MaxValue)

  final override def minBound: A =
    isoPosLong.reverseGet(PosLong.MinValue)

  final override def order: Order[A] =
    this

  final override def partialNext(a: A): Option[A] =
    refineV[Positive](isoPosLong.get(a).value + 1L).toOption.map(isoPosLong.reverseGet)

  final override def partialPrevious(a: A): Option[A] =
    refineV[Positive](isoPosLong.get(a).value - 1L).toOption.map(isoPosLong.reverseGet)

  // Order
  final override def compare(a: A, b: A): Int =
    isoPosLong.get(a).value.compare(isoPosLong.get(b).value)

  // Show
  final override def show(a: A): String =
    fromString.reverseGet(a)

  // Decoder
  final override def apply(c: HCursor): Decoder.Result[A] =
    c.as[String]
      .flatMap(s => fromString.getOption(s).toRight(DecodingFailure(s"Invalid GID: $s", Nil)))

  // Encoder
  final override def apply(a: A): Json =
    fromString.reverseGet(a).asJson

}

object Gid {
  def apply[A](implicit ev: Gid[A]): ev.type = ev

  def instance[A](tag: Char Refined Letter, toLong: A => PosLong, fromLong: PosLong => A): Gid[A] =
    new Gid[A](tag, Iso(toLong)(fromLong))
}

/**
 * Defines `<Entity>.Id` class, its `Gid` instance, and convenience methods.
 */
class WithGid(idTag: Char Refined Letter) {

  /** Id class for `<Entity>` */
  case class Id(value: PosLong) {
    override def toString: String =
      Gid[Id].show(this)
  }

  object Id {

    /** @group Typeclass Instances */
    implicit val GidId: Gid[Id] = Gid.instance(idTag, _.value, apply)

    /** Convenience method to construct from a Long */
    def fromLong(l: Long): Option[Id] = GidId.fromLong.getOption(l)

    /** Convenience method to construct from a String */
    def parse(s: String): Option[Id] = GidId.fromString.getOption(s)

    /** Allow pattern match style parsing */
    def unapply[T](s: String): Option[Id] = parse(s)
  }
}

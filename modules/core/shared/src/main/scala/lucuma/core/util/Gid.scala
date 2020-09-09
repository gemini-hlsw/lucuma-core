// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats._
import cats.implicits._
import monocle.{ Iso, Prism }
import scala.util.matching.Regex
import eu.timepit.refined.char.Letter
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined._
import eu.timepit.refined.api.Refined

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
) extends Order[A]
     with Show[A] {

  // We use this in a few places
  private final val TagString: String = tag.value.toString

  /** Gids have a unique String representation. */
  final val fromString: Prism[String, A] = {

    val R: Regex =
      raw"^$TagString-([1-9a-f][0-9a-f]*)$$".r

    def parse(s: String): Option[A] =
      s match {
        case R(s) =>
          for {
            signed <- Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(s, 16)).toOption
            pos    <- refineV[Positive](signed).toOption
          } yield isoPosLong.reverseGet(pos)
        case _ => None
      }

    def show(a: A): String =
      s"$TagString-${isoPosLong.get(a).value.toHexString}"

    Prism(parse)(show)

  }

  // Show
  final override def show(a: A): String =
    fromString.reverseGet(a)

  // Order
  final override def compare(a: A, b: A): Int =
    isoPosLong.get(a).value compare isoPosLong.get(b).value

}

object Gid {

  def apply[A](implicit ev: Gid[A]): ev.type = ev

  def instance[A](tag: Char Refined Letter, toLong: A => PosLong, fromLong: PosLong => A): Gid[A] =
    new Gid[A](tag, Iso(toLong)(fromLong))

}


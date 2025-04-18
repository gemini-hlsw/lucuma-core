// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Order
import cats.Show
import cats.syntax.either.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.char.Letter
import io.circe.*
import io.circe.syntax.*
import monocle.Iso
import monocle.Prism

import java.util.UUID
import scala.util.matching.Regex

/**
 * A typeclass representing a type-specific tag and a UUID. For example,
 * `a-f1006c8d-659d-44a4-a557-79879c3e8f95` identifies an atom ID with the leading `a` prefix.
 */
final class Uid[A](
  val tag:     Char Refined Letter,
  val isoUuid: Iso[A, UUID]
) extends Order[A]
    with Show[A]
    with Encoder[A]
    with Decoder[A] {

  // We use this in a few places
  private val TagString: String = tag.value.toString

  val regexPattern: String =
    raw"$TagString-([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})"

  private val R: Regex =
    regexPattern.r

  val fromString: Prism[String, A] = {

    def parse(s: String): Option[A] =
      s match {
        case R(u) =>
          Either
            .catchOnly[IllegalArgumentException](UUID.fromString(u))
            .toOption
            .map(isoUuid.reverseGet)
        case _    =>
          None
      }

    def show(a: A): String =
      s"$TagString-${isoUuid.get(a).toString.toLowerCase}"

    Prism(parse)(show)
  }

  // Order
  override def compare(a: A, b: A): Int =
    isoUuid.get(a).compareTo(isoUuid.get(b))

  // Show
  override def show(a: A): String =
    fromString.reverseGet(a)

  // Decoder
  override def apply(c: HCursor): Decoder.Result[A] =
    c.as[String]
      .flatMap(s => fromString.getOption(s).toRight(DecodingFailure(s"Invalid Uid: $s", Nil)))

  // Encoder
  override def apply(a: A): Json =
    fromString.reverseGet(a).asJson
}

/**
 * Defines `<Entity>.Id` class, its `Gid` instance, and convenience methods.
 */
object Uid {
  def apply[A](using ev: Uid[A]): ev.type = ev

  def instance[A](tag: Char Refined Letter, toUuid: A => UUID, fromUuid: UUID => A): Uid[A] =
    new Uid[A](tag, Iso(toUuid)(fromUuid))

}

class WithUid(idTag: Char Refined Letter) {

  case class Id(toUuid: UUID) {
    override def toString: String =
      Uid[Id].show(this)
  }

  object Id {
    given UidId: Uid[Id] =
      Uid.instance(idTag, _.toUuid, apply)

    def fromUuid(u: UUID): Id =
      UidId.isoUuid.reverseGet(u)

    def parse(s: String): Option[Id] =
      UidId.fromString.getOption(s)

    def unapply[T](s: String): Option[Id] =
      parse(s)

    given KeyDecoder[Id] = KeyDecoder.instance(UidId.fromString.getOption)

    given KeyEncoder[Id] = KeyEncoder.instance(UidId.fromString.reverseGet)
  }

}

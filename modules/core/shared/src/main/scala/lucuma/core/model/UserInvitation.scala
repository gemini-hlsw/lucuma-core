// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import lucuma.core.util.RefinedNewType
import monocle.Prism

import scala.util.matching.Regex

/**
 * An invitation consists of an id (a positive Long) and a cleartext body (a 96-char lowercase hex
 * string). Invitations have a canonical string representation `id.body` where `id` is in lowercase
 * hex, for example:
 * {{{
 * 10d.3b9e2adc5bffdac72f487a2760061bcfebc44037b69f2085c9a1ba10aa5d2d338421fc0d79f45cfd07666617ac4e2c89
 * }}}
 */
case class UserInvitation(id: UserInvitation.Id, body: String)

object UserInvitation {

  object Id extends RefinedNewType[Long, Positive] {

    /** Id from hex string. */
    val fromString: Prism[String, Id] =
      Prism[String, Id] { sid =>
        Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(sid, 16)).flatMap { n =>
          PosLong.from(n).map(Id.apply)
        } .toOption
      } { id =>
        id.value.value.toHexString
      }

    given Encoder[Id] = id => Json.fromString(Id.fromString.reverseGet(id))
    given Decoder[Id] = hc => hc.as[String].flatMap: s =>
      Id.fromString.getOption(s).toRight(DecodingFailure("Invalid invitation id: $s", hc.history))

  }
  type Id = Id.Type

  extension(invitation: UserInvitation)
    def token: String = fromString.reverseGet(invitation)

  private val R: Regex =
    raw"^([0-9a-f]{3,})\.([0-9a-f]{96})$$".r

  val fromString: Prism[String, UserInvitation] =
    Prism[String, UserInvitation] {
      case R(sid, body) => Id.fromString.getOption(sid).map(UserInvitation(_, body))
      case _            => None
     } { k =>
      s"${k.id.value.value.toHexString}.${k.body}"
    }

  given OrderUserInvitation: Order[UserInvitation] =
    Order.by(k => (k.id, k.body))

  given OrderingUserInvitation: Ordering[UserInvitation] =
    OrderUserInvitation.toOrdering

  given Encoder[UserInvitation] =
    Encoder.encodeString.contramap(fromString.reverseGet)

  given Decoder[UserInvitation] =
    Decoder.decodeString.emap(s => fromString.getOption(s).toRight(s"Invalid user invitation: $s"))
}

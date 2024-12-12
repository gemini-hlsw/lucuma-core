// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.util.Gid
import monocle.Prism

import scala.util.matching.Regex

/**
 * An invitation consists of a ProgramUser.Id and a cleartext body (a 96-char
 * lowercase hex string). Invitations have a canonical string representation
 * `id.body` where `id` is the Long portion of the ProgramUser.Id in lowercase
 * hex, for example:
 *
 * {{{
 * 10d.3b9e2adc5bffdac72f487a2760061bcfebc44037b69f2085c9a1ba10aa5d2d338421fc0d79f45cfd07666617ac4e2c89
 * }}}
 */
case class UserInvitation(id: ProgramUser.Id, body: String)

object UserInvitation:

  extension(invitation: UserInvitation)
    def token: String = fromString.reverseGet(invitation)

  private val R: Regex =
    raw"^([0-9a-f]{3,})\.([0-9a-f]{96})$$".r

  val fromString: Prism[String, UserInvitation] =
    Prism[String, UserInvitation] {
      case R(puid, body) =>
        val s = s"${Gid[ProgramUser.Id].tag.value}-${puid.dropWhile(_ === '0')}"
        ProgramUser.Id.parse(s).map(UserInvitation(_, body))
      case _             =>
        None
     } { k =>
      val id = f"${k.id.value.value.toHexString}%3s".replace(' ', '0')
      s"$id.${k.body}"
    }

  given OrderUserInvitation: Order[UserInvitation] =
    Order.by(k => (k.id, k.body))

  given OrderingUserInvitation: Ordering[UserInvitation] =
    OrderUserInvitation.toOrdering

  given Encoder[UserInvitation] =
    Encoder.encodeString.contramap(fromString.reverseGet)

  given Decoder[UserInvitation] =
    Decoder.decodeString.emap(s => fromString.getOption(s).toRight(s"Invalid user invitation: $s"))
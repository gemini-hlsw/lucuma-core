// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import cats.parse.*
import cats.parse.Parser.char
import cats.parse.Parser.charIn
import cats.parse.Rfc5234.*
import lucuma.core.math.Epoch
import lucuma.core.math.Epoch.Julian

/** Parser for [[lucuma.core.math.Epoch]]. */
trait EpochParsers {

  val besselian: Parser[Epoch.Scheme] =
    char('B').as[Epoch.Scheme](Epoch.Besselian).withContext("besselian")

  val julian: Parser[Epoch.Scheme] =
    char('J').as[Epoch.Scheme](Epoch.Julian).withContext("julian")

  /** Parser for an `Epoch.Scheme`. */
  val epochScheme: Parser[Epoch.Scheme] =
    (besselian | julian).withContext("epochScheme")

  private val miliyear: Parser[Int] = digit.rep(1, 3).map(_.toList.mkString("").padTo(3, '0').toInt)

  private val year: Parser[Int] = (charIn('1' to '3') ~ digit ~ digit ~ digit).string.map(_.toInt).filter(i => i >= 1900 && i <= 3000)

  /** Parser for an `Epoch`. */
  val epoch: Parser[Epoch] =
    (epochScheme ~ year ~ char('.').void ~ miliyear.?).map {
      case ((((s, y), _), m)) =>
        s.fromMilliyearsUnsafe(y * 1000 + m.getOrElse(0))
    }
      .withContext("epoch")

  /** Parser for an `Epoch` with a default Julian schemme. */
  val epochLenientNoScheme: Parser[Epoch] =
    (year ~ char('.').? ~ miliyear.?).map {
      case (((y, _), m)) =>
        Julian.fromMilliyearsUnsafe(y * 1000 + m.getOrElse(0))
      }
      .withContext("julianEpoch")

}

object EpochParsers extends EpochParsers

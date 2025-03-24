// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.csv

import cats.parse.Parser
import cats.parse.Parser.char
import cats.parse.Parser.charIn
import cats.parse.Rfc5234.*
import lucuma.core.math.Epoch
import lucuma.core.math.parser.EpochParsers
import lucuma.core.parser.TimeParsers

trait ImportEpochParsers {
  // Move to lucuma-core
  private val plainNumberEpoch: Parser[Epoch] =
    TimeParsers.year4.mapFilter(y =>
      if (y.getValue() >= 1900 && y.getValue() <= 3000)
        Epoch.Julian.fromIntMilliyears(y.getValue())
      else None
    )

  private val miliyear: Parser[Int] = digit.rep(1, 3).map(_.toList.mkString("").padTo(3, '0').toInt)

  private val year: Parser[Int] = (charIn('1' to '3') ~ digit ~ digit ~ digit).string
    .map(_.toInt)
    .filter(i => i >= 1900 && i <= 3000)

  /** Parser for an `Epoch`. */
  private val epoch: Parser[Epoch] =
    (EpochParsers.epochScheme ~ year ~ char('.').void.? ~ miliyear.?)
      .map { case ((((s, y), _), m)) =>
        s.fromMilliyearsUnsafe(y * 1000 + m.getOrElse(0))
      }
      .withContext("epoch")

  val epochParser: Parser[Epoch] =
    epoch | EpochParsers.epochLenientNoScheme | plainNumberEpoch

}

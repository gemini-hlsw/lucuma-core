// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import atto._
import cats.syntax.all._
import lucuma.core.math._
import lucuma.core.parser.MiscParsers

import Atto._

/** Parsers for [[lucuma.core.math.Coordinates]] and related types. */
trait CoordinateParsers {
  import AngleParsers.{ dms, hms }
  import MiscParsers.spaces1

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val ra: Parser[RightAscension] =
    hms.map(RightAscension(_)).named("ra")

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val dec: Parser[Declination] =
    dms
      .map(Declination.fromAngle.getOption)
      .flatMap {
        case Some(ra) => ok(ra)
        case None     => err[Declination]("Invalid Declination")
      }
      .named("dec")

  /** Parser for coordinates: HMS and DMS separated by spaces. */
  val coordinates: Parser[Coordinates] =
    (ra <~ spaces1, dec).mapN(Coordinates(_, _)).named("coordinates")

}
object CoordinateParsers extends CoordinateParsers

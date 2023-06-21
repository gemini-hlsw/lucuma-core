// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.parser

import cats.parse.Rfc5234.*
import cats.parse.*
import lucuma.core.math.*

/** Parsers for [[lucuma.core.math.Coordinates]] and related types. */
trait CoordinateParsers:
  import AngleParsers.{ dms, hms }

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val ra: Parser0[RightAscension] =
    hms.map(RightAscension(_)).withContext("ra")

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val dec: Parser0[Declination] =
    dms.map(Declination.fromAngle.getOption)
      .flatMap {
        case Some(dec) => Parser.pure(dec)
        case None     => Parser.failWith[Declination]("Invalid Declination")
      }
      .withContext("dec")

  /** Parser for coordinates: HMS and DMS separated by spaces. */
  val coordinates: Parser0[Coordinates] =
    (ra ~ sp.rep.?.void ~ dec).map{ case ((ra, _), dec) => Coordinates(ra, dec)}

object CoordinateParsers extends CoordinateParsers

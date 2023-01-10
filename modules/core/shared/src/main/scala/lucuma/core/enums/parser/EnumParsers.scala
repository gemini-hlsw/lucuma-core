// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums.parser

import cats.parse.Parser.*
import cats.parse.*
import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.util.Enumerated

/** Parsers for `gem.enum` data types. */
trait EnumParsers {

  /** Parser for an `Enumerated` type, based on some string property. */
  def enumBy[A](f: A => String)(using ev: Enumerated[A]): Parser[A] =
    oneOf(ev.all.map(a => string(f(a)).as(a))).withContext("enumBy(...)")

  /** Parser for `Site` based on `shortName` like `GS`. */
  val site: Parser[Site] =
    enumBy[Site](_.shortName).withContext("Site")

  /** Parser for `ProgramType` based on `shortName` like `SV`. */
  val programType: Parser[ProgramType] =
    enumBy[ProgramType](_.shortName).withContext("ProgramType")

  /** Parser for `Half` based on `tag` like `A`. */
  val half: Parser[Half] =
    enumBy[Half](_.tag).withContext("Half")

}
object EnumParsers extends EnumParsers

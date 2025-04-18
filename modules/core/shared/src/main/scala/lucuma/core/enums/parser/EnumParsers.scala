// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums
package parser

import cats.parse.*
import cats.parse.Parser.*
import lucuma.core.util.Enumerated

/** Parsers for `gem.enum` data types. */
trait EnumParsers {

  /** Parser for an `Enumerated` type, based on some string property. */
  def enumBy[A](f: A => String)(using ev: Enumerated[A]): Parser[A] =
    oneOf(ev.all.map(a => string(f(a)).as(a))).withContext("enumBy(...)")

  /** Parser for `Half` based on `tag` like `A`. */
  val half: Parser[Half] =
    (char('A').as(Half.A) | char('B').as(Half.B)).withContext("Half")

  /** Parser for `Instrument` */
  val instrument: Parser[Instrument] =
    enumBy[Instrument](_.tag)

  /** Parser for `ProgramType` based on `shortName` like `XPL`. */
  val programType: Parser[ProgramType] =
    enumBy[ProgramType](_.abbreviation).withContext("ProgramType")

  /** Parser for `ScienceSubtype` based on the single character abbreviation. */
  val scienceSubtype: Parser[ScienceSubtype] =
    enumBy[ScienceSubtype](_.letter.toString)

  /** Parser for `Site` based on `shortName` like `GS`. */
  val site: Parser[Site] =
    enumBy[Site](_.shortName).withContext("Site")

}
object EnumParsers extends EnumParsers

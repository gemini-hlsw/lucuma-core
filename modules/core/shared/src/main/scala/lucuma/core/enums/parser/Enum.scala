// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums.parser

import atto._
import cats.syntax.all._
import lucuma.core.enums._
import lucuma.core.util.Enumerated

import Atto._

/** Parsers for `gem.enum` data types. */
trait EnumParsers {

  /** Parser for an `Enumerated` type, based on some string property. */
  def enumBy[A](f: A => String)(implicit ev: Enumerated[A]): Parser[A] =
    choice(ev.all.map(a => string(f(a)).as(a))).named("enumBy(...)")

  /** Parser for `Site` based on `shortName` like `GS`. */
  val site: Parser[Site] =
    enumBy[Site](_.shortName).namedOpaque("Site")

  /** Parser for `ProgramType` based on `shortName` like `SV`. */
  val programType: Parser[ProgramType] =
    enumBy[ProgramType](_.shortName).namedOpaque("ProgramType")

  /** Parser for `Half` based on `tag` like `A`. */
  val half: Parser[Half] =
    enumBy[Half](_.tag).namedOpaque("Half")

}
object EnumParsers extends EnumParsers

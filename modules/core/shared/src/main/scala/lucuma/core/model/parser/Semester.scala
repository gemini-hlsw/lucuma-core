// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package parser

import cats.syntax.all._
import atto._, Atto._
import lucuma.core.enum.parser.EnumParsers._
import lucuma.core.parser.TimeParsers._

/** Parser for [[gem.Semester]]. */
trait SemesterParsers {

  /** Parser for a full-year `Semester` like `2015A`. */
  val semester: Parser[Semester] =
    (year4, half).mapN(Semester.apply).named("semester")

}
object SemesterParsers extends SemesterParsers

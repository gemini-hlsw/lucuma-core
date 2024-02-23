// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package parser

import cats.parse.Parser
import cats.parse.Parser.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Half
import lucuma.core.parser.MiscParsers.intN
import lucuma.core.parser.MiscParsers.posInt

import java.time.DateTimeException
import java.time.Year
import scala.util.control.Exception

trait ReferenceParsers {

//  import lucuma.core.enums.parser.EnumParsers.instrument
//  import lucuma.core.enums.parser.EnumParsers.scienceSubtype

  val G: Parser[Unit] =
    char('G').void.withContext("g")

  val dash: Parser[Unit] =
    char('-').void.withContext("-")

  val semesterYear: Parser[Year] =
    intN(4).mapFilter {
      case yr if yr >= 2000 =>
        Exception.catching(classOf[DateTimeException]).opt {
          Year.of(yr)
        }
      case _ => none
    }

  val half: Parser[Half] =
    char('A').as(Half.A) | char('B').as(Half.B)

  val semester: Parser[Semester] =
    (semesterYear ~ half).map(Semester.apply)

  val index: Parser[PosInt] =
    char('0').rep0.with1 *> posInt

  val proposal: Parser[ProposalReference] =
    (semester.between(string("G-"), dash) ~ index).map { (semester, index) =>
      ProposalReference(semester, index)
    }
}

object ReferenceParsers extends ReferenceParsers

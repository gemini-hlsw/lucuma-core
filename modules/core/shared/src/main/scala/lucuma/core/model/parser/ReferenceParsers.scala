// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package parser

import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Instrument

/**
 * Parsers shared between ProposalReference and ProgramReference.
 */
trait ReferenceParsers {

  import lucuma.core.enums.parser.EnumParsers.enumBy
  import lucuma.core.parser.MiscParsers.posInt

  val G: Parser[Unit] =
    char('G').void.withContext("g")

  val dash: Parser[Unit] =
    char('-').void.withContext("-")

  val index: Parser[PosInt] =
    char('0').rep0.with1 *> posInt

  val instrumentByReferenceName: Parser[Instrument] =
    enumBy[Instrument](_.referenceName.value)

}

object ReferenceParsers extends ReferenceParsers

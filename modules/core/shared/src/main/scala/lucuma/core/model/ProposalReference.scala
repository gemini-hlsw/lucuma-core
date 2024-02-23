// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model


import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.optics.Format


case class ProposalReference(semester: Semester, index: PosInt) {
  def label: String =
    f"G-${semester.format}-$index%04d"
}

object ProposalReference {

  object parse {

    import parser.ReferenceParsers.*
    import Semester.parse.semester

    val proposal: Parser[ProposalReference] =
      (semester.between(string("G-"), dash) ~ index).map { (semester, index) =>
        ProposalReference(semester, index)
      }

  }

  given Order[ProposalReference] =
    Order.by { a => (a.semester, a.index.value) }

  val fromString: Format[String, ProposalReference] =
    Format(s => parse.proposal.parseAll(s).toOption, _.label)

  given Decoder[ProposalReference] =
    Decoder.decodeString.emap { s =>
      fromString
        .getOption(s)
        .toRight(s"Could not parse '$s' as a ProposalReference.")
    }

  given Encoder[ProposalReference] =
    Encoder.instance(_.label.asJson)

}
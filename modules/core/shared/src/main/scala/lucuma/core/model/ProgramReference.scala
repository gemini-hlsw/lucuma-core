// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import cats.parse.Parser
import cats.parse.Parser.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.cats.given
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.optics.Format
import monocle.Prism


sealed trait ProgramReference extends Product with Serializable {
  /**
   * The associated program type.  Each program reference corresponds to a
   * particular program type.
   */
  def programType: ProgramType

  /**
   * Formatted program reference.
   */
  def label: String
}

/**
 * Program references that are paticular to a specific instrument.
 */
sealed trait InstrumentProgramReference extends ProgramReference {
  def instrument: Instrument
}

/**
 * Program references associated with a particular semester.
 */
sealed trait SemesterlyProgramReference extends ProgramReference {
  def semester: Semester
}

/**
 * Semesterly program references tied to specific instruments.  These are all
 * formatted in the sme way.
 */
sealed trait SemesterlyInstrumentProgramReference extends InstrumentProgramReference with SemesterlyProgramReference {
  def index: PosInt

  override def label: String =
    f"G-${semester.format}-${programType.abbreviation}-${instrument.referenceName}-$index%02d"
}

object ProgramReference {

  /**
   * Library description type, which constrains the allowable characters in the
   * String.
   */
  type Description = String Refined MatchesRegex["""^[A-Z0-9]+"""]
  object Description extends RefinedTypeOps[Description, String]

  case class Calibration(semester: Semester, instrument: Instrument, index: PosInt) extends SemesterlyInstrumentProgramReference {
    override def programType: ProgramType = ProgramType.Calibration
  }

  object Calibration {

    given Order[Calibration] =
      Order.by { a => (a.semester, a.instrument, a.index) }

    val fromString: Format[String, Calibration] =
      Format(s => parse.calibration.parseAll(s).toOption, _.label)

  }

  case class Commissioning(semester: Semester, instrument: Instrument, index: PosInt) extends SemesterlyInstrumentProgramReference {
    override def programType: ProgramType = ProgramType.Commissioning
  }

  object Commissioning {

    given Order[Commissioning] =
      Order.by { a => (a.semester, a.instrument, a.index) }

    val fromString: Format[String, Commissioning] =
      Format(s => parse.commissioning.parseAll(s).toOption, _.label)

  }

  case class Engineering(semester: Semester, instrument: Instrument, index: PosInt) extends SemesterlyInstrumentProgramReference {
    override def programType: ProgramType = ProgramType.Engineering
  }

  object Engineering {

    given Order[Engineering] =
      Order.by { a => (a.semester, a.instrument, a.index) }

    val fromString: Format[String, Engineering] =
      Format(s => parse.engineering.parseAll(s).toOption, _.label)

  }

  case class Example(instrument: Instrument) extends InstrumentProgramReference {
    override def programType: ProgramType = ProgramType.Example

    override def label: String =
      s"G-${programType.abbreviation}-${instrument.referenceName}"
  }

  object Example {

    given Order[Example] =
      Order.by(_.instrument)

    val fromString: Prism[String, Example] =
      Prism[String, Example](s => parse.example.parseAll(s).toOption)(_.label)

  }

  case class Library(instrument: Instrument, description: Description) extends InstrumentProgramReference {
    override def programType: ProgramType = ProgramType.Library

    override def label: String =
      s"G-${programType.abbreviation}-${instrument.referenceName}-${description.value}"
  }

  object Library {

    given Order[Library] =
      Order.by { a => (a.instrument, a.description.value) }

    val fromString: Prism[String, Library] =
      Prism[String, Library](s => parse.library.parseAll(s).toOption)(_.label)

  }

  case class Monitoring(semester: Semester, instrument: Instrument, index: PosInt) extends SemesterlyInstrumentProgramReference {
    override def programType: ProgramType = ProgramType.Monitoring
  }

  object Monitoring {

    given Order[Monitoring] =
      Order.by { a => (a.semester, a.instrument, a.index) }

    val fromString: Format[String, Monitoring] =
      Format(s => parse.monitoring.parseAll(s).toOption, _.label)

  }

  case class Science(proposal: ProposalReference, scienceSubtype: ScienceSubtype) extends SemesterlyProgramReference {
    override def programType: ProgramType = ProgramType.Science

    override def semester: Semester = proposal.semester

    override def label: String =
      s"${proposal.label}-${scienceSubtype.letter}"
  }

  object Science {

    given Order[Science] =
      Order.by { a => (a.proposal, a.scienceSubtype) }

    val fromString: Format[String, Science] =
      Format(s => parse.program.parseAll(s).toOption, _.label)
  }

  object parse {

    import lucuma.core.enums.parser.EnumParsers.scienceSubtype
    import parser.ReferenceParsers.*
    import Semester.parse.semester

    val description: Parser[Description] =
      charsWhile { c => (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') }
        .mapFilter(s => Description.from(s).toOption)

    def semesterInstrumentIndex[A](abbr: String)(f: (Semester, Instrument, PosInt) => A): Parser[A] =
      (semester.surroundedBy(dash).between(G, string(abbr)) ~ instrumentByReferenceName.surroundedBy(dash) ~ index)
        .map { case ((semester, instrument), index) => f(semester, instrument, index) }

    val calibration: Parser[Calibration] =
      semesterInstrumentIndex(ProgramType.Calibration.abbreviation)(Calibration.apply)

    val commissioning: Parser[Commissioning] =
      semesterInstrumentIndex(ProgramType.Commissioning.abbreviation)(Commissioning.apply)

    val engineering: Parser[Engineering] =
      semesterInstrumentIndex(ProgramType.Engineering.abbreviation)(Engineering.apply)

    val example: Parser[Example] =
      (string(s"G-${ProgramType.Example.abbreviation}-") *> instrumentByReferenceName).map { instrument =>
        Example(instrument)
      }

    val library: Parser[Library] =
      (instrumentByReferenceName.between(string(s"G-${ProgramType.Library.abbreviation}-"), dash) ~ description)
        .map { case (instrument, description) => Library(instrument, description) }

    val monitoring: Parser[Monitoring] =
      semesterInstrumentIndex(ProgramType.Monitoring.abbreviation)(Monitoring.apply)

    val program: Parser[Science] =
      ((ProposalReference.parse.proposal <* dash) ~ scienceSubtype).map { case (proposal, scienceSubtype) =>
        Science(proposal, scienceSubtype)
      }

    val programReference: Parser[ProgramReference] =
      oneOf(
        parse.program.backtrack       ::
        parse.calibration.backtrack   ::
        parse.commissioning.backtrack ::
        parse.engineering.backtrack   ::
        parse.example.backtrack       ::
        parse.library.backtrack       ::
        parse.monitoring              ::
        Nil
      )
  }

  val fromString: Format[String, ProgramReference] =
    Format(s => parse.programReference.parseAll(s).toOption, _.label)

  given Decoder[ProgramReference] =
    Decoder.decodeString.emap { s =>
      fromString
        .getOption(s)
        .toRight(s"Could not parse '$s' as a ProgramReference.")
    }

  given Encoder[ProgramReference] =
    Encoder.instance(_.label.asJson)

  given Order[ProgramReference] =
    Order.from {
      case (a @ Calibration(_, _, _),   b @ Calibration(_, _, _))   => Order.compare(a, b)
      case (a @ Commissioning(_, _, _), b @ Commissioning(_, _, _)) => Order.compare(a, b)
      case (a @ Engineering(_, _, _),   b @ Engineering(_, _, _))   => Order.compare(a, b)
      case (a @ Example(_),             b @ Example(_))             => Order.compare(a, b)
      case (a @ Library(_, _),          b @ Library(_, _))          => Order.compare(a, b)
      case (a @ Monitoring(_, _, _),    b @ Monitoring(_, _, _))    => Order.compare(a, b)
      case (a @ Science(_, _),          b @ Science(_, _))          => Order.compare(a, b)
      case (a, b) => Order.compare(a.programType, b.programType)
    }

  given Ordering[ProgramReference] =
    Order[ProgramReference].toOrdering

}
// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceSubtype
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen


trait ArbProgramReference extends ArbReference {

  import ArbEnumerated.given
  import ArbSemester.given
  import ArbProposalReference.given

  import ProgramReference.*

  given Arbitrary[Calibration] =
    Arbitrary {
      for {
        s <- arbitrary[Semester]
        i <- arbitrary[Instrument]
        n <- arbitraryIndex
      } yield Calibration(s, i, PosInt.unsafeFrom(n))
    }

  given Cogen[Calibration] =
    Cogen[(Semester, Instrument, Int)].contramap { a => (
      a.semester,
      a.instrument,
      a.index.value
    )}

  val calibrationStrings: Gen[String] =
    referenceStrings[Calibration](_.label)

  given Arbitrary[Engineering] =
    Arbitrary {
      for {
        s <- arbitrary[Semester]
        i <- arbitrary[Instrument]
        n <- arbitraryIndex
      } yield Engineering(s, i, PosInt.unsafeFrom(n))
    }

  given Cogen[Engineering] =
    Cogen[(Semester, Instrument, Int)].contramap { a => (
      a.semester,
      a.instrument,
      a.index.value
    )}

  val engineeringStrings: Gen[String] =
    referenceStrings[Engineering](_.label)

  given Arbitrary[Example] =
    Arbitrary {
      arbitrary[Instrument].map(Example.apply)
    }

  given Cogen[Example] =
    Cogen[Instrument].contramap(_.instrument)

  given Arbitrary[Description] =
    Arbitrary {
      for {
        c <- Gen.alphaNumChar.map(_.toUpper)
        s <- Gen.alphaNumStr.map(_.toUpperCase)
        d  = s"$c$s"
      } yield Description.unsafeFrom(d)
    }

  given Cogen[Description] =
    Cogen[String].contramap(_.value)

  given Arbitrary[Library] =
    Arbitrary {
      for {
        i <- arbitrary[Instrument]
        d <- arbitrary[Description]
      } yield Library(i, d)
    }

  given Cogen[Library] =
    Cogen[(Instrument, Description)].contramap { a => (
      a.instrument,
      a.description
    )}

  given Arbitrary[Science] =
    Arbitrary {
      for {
        p <- arbitrary[ProposalReference]
        t <- arbitrary[ScienceSubtype]
      } yield ProgramReference.Science(p, t)
    }

  given Cogen[Science] =
    Cogen[(ProposalReference, ScienceSubtype)].contramap { a => (
      a.proposal,
      a.scienceSubtype
    )}

  val scienceStrings: Gen[String] =
    referenceStrings[Science](_.label)

  given Arbitrary[ProgramReference] =
    Arbitrary {
      Gen.oneOf[ProgramReference](
        arbitrary[Calibration],
        arbitrary[Engineering],
        arbitrary[Example],
        arbitrary[Library],
        arbitrary[Science]
      )
    }

  given Cogen[ProgramReference] =
    Cogen[String].contramap(_.label)

  val programReferenceStrings: Gen[String] =
    referenceStrings[ProgramReference](_.label)

}

object ArbProgramReference extends ArbProgramReference

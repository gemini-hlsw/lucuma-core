// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import edu.gemini.tac.qengine.p1.Proposal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Half
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceBand.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import munit.FunSuite
import org.junit.Assert
import lucuma.core.model.ProposalType
import lucuma.core.model.IntPercent
import lucuma.core.util.TimeSpan

class QueueEngineBandProblemsTest extends FunSuite {
  import QueueEngineBandProblems._

  // A proposal with no data. We .copy it to make test proposals.
  private val P: Proposal =
    Proposal(
      ProposalReference(Semester(YearInt.unsafeFrom(2026), Half.A), PosInt.unsafeFrom(1)),
      allocations       = null,
      tpe               = null,
      obsList           = null,
      band3Observations = null,
    )

  // Test that the given problem check fails with the the given proposal and per-band expectations.
  private def testRule(problem: Problem, proposal: Proposal)(expect: PartialFunction[ScienceBand, String]): Unit =
    ScienceBand.values.foreach(b => Assert.assertEquals(expect.lift(b), problem.lift((proposal, b))))

  test("testClassicalNotInBand1"):
    testRule(ClassicalNotInBand1, P.copy(tpe = ProposalType.Classical(IntPercent.unsafeFrom(100), Nil))) { // TODO
      case Band2 => "Classical proposal in Band2"
      case Band3 => "Classical proposal in Band3"
      case Band4 => "Classical proposal in Band4"
    }

  test("testNoObsInBand124"):
    testRule(NoObsInBand, P.copy(obsList = Nil, band3Observations = List(null))) {
      case Band1 => ("No observations were found for Band1")
      case Band2 => ("No observations were found for Band2")
      case Band4 => ("No observations were found for Band4")
    }

  test("testNoObsInBand3"):
    testRule(NoObsInBand, P.copy(obsList = List(null), band3Observations = Nil)) {
      case Band3 => "No observations were found for Band3"
    }

  test("testLpInBand3Or4"):
    testRule(LpInBand3Or4, P.copy(tpe = ProposalType.LargeProgram(ToOActivation.None, IntPercent.unsafeFrom(100), IntPercent.unsafeFrom(100), TimeSpan.Min))) { // TODO
      case Band3 => "LP proposal in Band3"
      case Band4 => "LP proposal in Band4"
    }

  test("testRapidTooOutsideBand1"):
    testRule(RapidTooOutsideBand1, P.copy(tpe = ProposalType.Queue(ToOActivation.Rapid, IntPercent.unsafeFrom(100), Nil))) { // TODO
      case Band2 => "Rapid TOO proposal in Band2"
      case Band3 => "Rapid TOO proposal in Band3"
      case Band4 => "Rapid TOO proposal in Band4"
    }

  test("testStandardTooOutsideBand12"):
    testRule(StandardTooOutsideBand12, P.copy(tpe = ProposalType.Queue(ToOActivation.Standard, IntPercent.unsafeFrom(100), Nil))) { // TODO
      case Band3 => "Standard TOO proposal in Band3"
      case Band4 => "Standard TOO proposal in Band4"
    }

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceBand.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import munit.FunSuite
import org.junit.Assert

class QueueEngineBandProblemsTest extends FunSuite {
  import QueueEngineBandProblems._

  // A proposal with no data. We .copy it to make test proposals.
  private val P: Proposal =
    Proposal(
      ntac              = null,
      site              = null,
      mode              = null,
      too               = null,
      obsList           = null,
      band3Observations = null,
      isPoorWeather     = false,
      piName            = null,
      piEmail           = null,
      p1proposal        = null,
      p1mutableProposal = null,
      p1xmlFile         = null
    )

  // Test that the given problem check fails with the the given proposal and per-band expectations.
  private def testRule(problem: Problem, proposal: Proposal)(expect: PartialFunction[ScienceBand, String]): Unit =
    ScienceBand.values.foreach(b => Assert.assertEquals(expect.lift(b), problem.lift((proposal, b))))

  test("testClassicalNotInBand1"):
    testRule(ClassicalNotInBand1, P.copy(mode = ScienceSubtype.Classical)) {
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
    testRule(LpInBand3Or4, P.copy(mode = ScienceSubtype.LargeProgram)) {
      case Band3 => "LP proposal in Band3"
      case Band4 => "LP proposal in Band4"
    }

  test("testRapidTooOutsideBand1"):
    testRule(RapidTooOutsideBand1, P.copy(too = ToOActivation.Rapid)) {
      case Band2 => "Rapid TOO proposal in Band2"
      case Band3 => "Rapid TOO proposal in Band3"
      case Band4 => "Rapid TOO proposal in Band4"
    }

  test("testStandardTooOutsideBand12"):
    testRule(StandardTooOutsideBand12, P.copy(too = ToOActivation.Standard)) {
      case Band3 => "Standard TOO proposal in Band3"
      case Band4 => "Standard TOO proposal in Band4"
    }

}
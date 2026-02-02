// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import edu.gemini.tac.qengine.p1.Mode
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.p1.QueueBand
import edu.gemini.tac.qengine.p1.QueueBand.*
import edu.gemini.tac.qengine.p1.Too
import org.junit.Assert
import org.junit.Test

class QueueEngineBandProblemsTest {
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
  private def testRule(problem: Problem, proposal: Proposal)(expect: PartialFunction[QueueBand, String]): Unit =
    QueueBand.values.foreach(b => Assert.assertEquals(expect.lift(b), problem.lift((proposal, b))))

  @Test def testClassicalNotInBand1: Unit =
    testRule(ClassicalNotInBand1, P.copy(mode = Mode.Classical)) {
      case QBand2 => "Classical proposal in band 2"
      case QBand3 => "Classical proposal in band 3"
      case QBand4 => "Classical proposal in band 4"
    }

  @Test def testNoObsInBand124: Unit =
    testRule(NoObsInBand, P.copy(obsList = Nil, band3Observations = List(null))) {
      case QBand1 => ("No observations were found for Band 1")
      case QBand2 => ("No observations were found for Band 2")
      case QBand4 => ("No observations were found for Band 4")
    }

  @Test def testNoObsInBand3: Unit =
    testRule(NoObsInBand, P.copy(obsList = List(null), band3Observations = Nil)) {
      case QBand3 => "No observations were found for Band 3"
    }

  @Test def testLpInBand3Or4: Unit =
    testRule(LpInBand3Or4, P.copy(mode = Mode.LargeProgram)) {
      case QBand3 => "LP proposal in Band 3"
      case QBand4 => "LP proposal in Band 4"
    }

  @Test def testRapidTooOutsideBand1: Unit =
    testRule(RapidTooOutsideBand1, P.copy(too = Too.rapid)) {
      case QBand2 => "Rapid TOO proposal in Band 2"
      case QBand3 => "Rapid TOO proposal in Band 3"
      case QBand4 => "Rapid TOO proposal in Band 4"
    }

  @Test def testStandardTooOutsideBand12: Unit =
    testRule(StandardTooOutsideBand12, P.copy(too = Too.standard)) {
      case QBand3 => "Standard TOO proposal in band 3"
      case QBand4 => "Standard TOO proposal in band 4"
    }

}
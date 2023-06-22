// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.ProposalClass.*
import lucuma.core.model.arb.*
import lucuma.core.util.arb.ArbTimeSpan
import monocle.law.discipline.*
import munit.*

class ProposalClassSuite extends DisciplineSuite {
  import ArbTimeSpan.given
  import ArbProposalClass.given

  // Laws for instances
  checkAll("Eq[Classical]", EqTests[Classical].eqv)
  checkAll("Classical.minPercentTime", LensTests(Classical.minPercentTime))

  checkAll("Eq[DemoScience]", EqTests[DemoScience].eqv)
  checkAll("DemoScience.minPercentTime", LensTests(DemoScience.minPercentTime))

  checkAll("Eq[DirectorsTime]", EqTests[DirectorsTime].eqv)
  checkAll("DirectorsTime.minPercentTime", LensTests(DirectorsTime.minPercentTime))

  checkAll("Eq[Exchange]", EqTests[Exchange].eqv)
  checkAll("Exchange.minPercentTime", LensTests(Exchange.minPercentTime))

  checkAll("Eq[FastTurnaround]", EqTests[FastTurnaround].eqv)
  checkAll("FastTurnaround.minPercentTime", LensTests(FastTurnaround.minPercentTime))

  checkAll("Eq[PoorWeather]", EqTests[PoorWeather].eqv)
  checkAll("PoorWeather.minPercentTime", LensTests(PoorWeather.minPercentTime))

  checkAll("Eq[Queue]", EqTests[Queue].eqv)
  checkAll("Queue.minPercentTime", LensTests(Queue.minPercentTime))

  checkAll("Eq[SystemVerification]", EqTests[SystemVerification].eqv)
  checkAll("SystemVerification.minPercentTime", LensTests(SystemVerification.minPercentTime))

  checkAll("Eq[LargeProgram]", EqTests[LargeProgram].eqv)
  checkAll("LargeProgram.minPercentTime", LensTests(LargeProgram.minPercentTime))
  checkAll("LargeProgram.minPercentTotalTime", LensTests(LargeProgram.minPercentTotalTime))
  checkAll("LargeProgram.totalTime", LensTests(LargeProgram.totalTime))

  checkAll("Eq[Intensive]", EqTests[Intensive].eqv)
  checkAll("Intensive.minPercentTime", LensTests(Intensive.minPercentTime))
  checkAll("Intensive.minPercentTotalTime", LensTests(Intensive.minPercentTotalTime))
  checkAll("Intensive.totalTime", LensTests(Intensive.totalTime))

  // Laws for ProposalClass trait
  checkAll("Eq[ProposalClass]", EqTests[ProposalClass].eqv)
  checkAll("ProposalClass.minPercentTime", LensTests(ProposalClass.minPercentTime))
  checkAll("ProposalClass.minPercentTotalTime", OptionalTests(ProposalClass.minPercentTotalTime))
  checkAll("ProposalClass.totalTime", OptionalTests(ProposalClass.totalTime))

  checkAll("ProposalClass.classical", PrismTests(ProposalClass.classical))
  checkAll("ProposalClass.demoScience", PrismTests(ProposalClass.demoScience))
  checkAll("ProposalClass.directorsTime", PrismTests(ProposalClass.directorsTime))
  checkAll("ProposalClass.exchange", PrismTests(ProposalClass.exchange))
  checkAll("ProposalClass.fastTurnaround", PrismTests(ProposalClass.fastTurnaround))
  checkAll("ProposalClass.poorWeather", PrismTests(ProposalClass.poorWeather))
  checkAll("ProposalClass.queue", PrismTests(ProposalClass.queue))
  checkAll("ProposalClass.systemVerification", PrismTests(ProposalClass.systemVerification))
  checkAll("ProposalClass.largeProgram", PrismTests(ProposalClass.largeProgram))
  checkAll("ProposalClass.intensive", PrismTests(ProposalClass.intensive))
}

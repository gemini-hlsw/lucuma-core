// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.IntPercent
import lucuma.core.model.ProposalClass
import lucuma.core.model.ProposalClass.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbProposalClass {
  import ArbTimeSpan.given

  given Arbitrary[Classical] =
    Arbitrary(arbitrary[IntPercent].map(Classical(_)))

  given Cogen[Classical] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[DemoScience] =
    Arbitrary(arbitrary[IntPercent].map(DemoScience(_)))

  given Cogen[DemoScience] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[DirectorsTime] =
    Arbitrary(arbitrary[IntPercent].map(DirectorsTime(_)))

  given Cogen[DirectorsTime] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[Exchange] =
    Arbitrary(arbitrary[IntPercent].map(Exchange(_)))

  given Cogen[Exchange] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[FastTurnaround] =
    Arbitrary(arbitrary[IntPercent].map(FastTurnaround(_)))

  given Cogen[FastTurnaround] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[PoorWeather] =
    Arbitrary(arbitrary[IntPercent].map(PoorWeather(_)))

  given Cogen[PoorWeather] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[Queue] =
    Arbitrary(arbitrary[IntPercent].map(Queue(_)))

  given Cogen[Queue] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[SystemVerification] =
    Arbitrary(arbitrary[IntPercent].map(SystemVerification(_)))

  given Cogen[SystemVerification] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  given Arbitrary[LargeProgram] =
    Arbitrary {
      for {
        minPct    <- arbitrary[IntPercent]
        minTotPct <- arbitrary[IntPercent]
        totalTime <- arbitrary[TimeSpan]
      } yield LargeProgram(minPct, minTotPct, totalTime)
    }

  given Cogen[LargeProgram] =
    Cogen[(IntPercent, IntPercent, TimeSpan)].contramap(p =>
      (p.minPercentTime, p.minPercentTotalTime, p.totalTime)
    )

  given Arbitrary[Intensive] =
    Arbitrary {
      for {
        minPct    <- arbitrary[IntPercent]
        minTotPct <- arbitrary[IntPercent]
        totalTime <- arbitrary[TimeSpan]
      } yield Intensive(minPct, minTotPct, totalTime)
    }

  given Cogen[Intensive] =
    Cogen[(IntPercent, IntPercent, TimeSpan)].contramap(p =>
      (p.minPercentTime, p.minPercentTotalTime, p.totalTime)
    )

  given Arbitrary[ProposalClass] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[Classical],
        arbitrary[DemoScience],
        arbitrary[DirectorsTime],
        arbitrary[Exchange],
        arbitrary[FastTurnaround],
        arbitrary[PoorWeather],
        arbitrary[Queue],
        arbitrary[SystemVerification],
        arbitrary[LargeProgram],
        arbitrary[Intensive]
      )
    )

  given Cogen[ProposalClass] =
    Cogen[
      (
        Option[Classical],
        Option[DemoScience],
        Option[DirectorsTime],
        Option[Exchange],
        Option[FastTurnaround],
        Option[PoorWeather],
        Option[Queue],
        Option[SystemVerification],
        Option[LargeProgram],
        Option[Intensive]
      )
    ].contramap(p =>
      (
        ProposalClass.classical.getOption(p),
        ProposalClass.demoScience.getOption(p),
        ProposalClass.directorsTime.getOption(p),
        ProposalClass.exchange.getOption(p),
        ProposalClass.fastTurnaround.getOption(p),
        ProposalClass.poorWeather.getOption(p),
        ProposalClass.queue.getOption(p),
        ProposalClass.systemVerification.getOption(p),
        ProposalClass.largeProgram.getOption(p),
        ProposalClass.intensive.getOption(p)
      )
    )
}

object ArbProposalClass extends ArbProposalClass

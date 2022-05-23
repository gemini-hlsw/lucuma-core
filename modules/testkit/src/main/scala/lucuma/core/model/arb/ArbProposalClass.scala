// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import eu.timepit.refined.scalacheck.all._
import lucuma.core.arb.ArbTime
import lucuma.core.math.units.IntPercent
import lucuma.core.model.ProposalClass
import lucuma.core.model.ProposalClass._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import java.time.Duration

trait ArbProposalClass {
  import ArbTime._

  implicit val arbClassical: Arbitrary[Classical] =
    Arbitrary(arbitrary[IntPercent].map(Classical(_)))

  implicit val cogClassical: Cogen[Classical] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbDemoScience: Arbitrary[DemoScience] =
    Arbitrary(arbitrary[IntPercent].map(DemoScience(_)))

  implicit val cogDemoScience: Cogen[DemoScience] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbDirectorsTime: Arbitrary[DirectorsTime] =
    Arbitrary(arbitrary[IntPercent].map(DirectorsTime(_)))

  implicit val cogDirectorsTime: Cogen[DirectorsTime] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbExchange: Arbitrary[Exchange] =
    Arbitrary(arbitrary[IntPercent].map(Exchange(_)))

  implicit val cogExchange: Cogen[Exchange] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbFastTurnaround: Arbitrary[FastTurnaround] =
    Arbitrary(arbitrary[IntPercent].map(FastTurnaround(_)))

  implicit val cogFastTurnaround: Cogen[FastTurnaround] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbPoorWeather: Arbitrary[PoorWeather] =
    Arbitrary(arbitrary[IntPercent].map(PoorWeather(_)))

  implicit val cogPoorWeather: Cogen[PoorWeather] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbQueue: Arbitrary[Queue] =
    Arbitrary(arbitrary[IntPercent].map(Queue(_)))

  implicit val cogQueue: Cogen[Queue] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbSystemVerification: Arbitrary[SystemVerification] =
    Arbitrary(arbitrary[IntPercent].map(SystemVerification(_)))

  implicit val cogSystemVerification: Cogen[SystemVerification] =
    Cogen[IntPercent].contramap(_.minPercentTime)

  implicit val arbLargeProgram: Arbitrary[LargeProgram] =
    Arbitrary {
      for {
        minPct    <- arbitrary[IntPercent]
        minTotPct <- arbitrary[IntPercent]
        totalTime <- arbitrary[Duration]
      } yield LargeProgram(minPct, minTotPct, totalTime)
    }

  implicit val cogLargeProgram: Cogen[LargeProgram] =
    Cogen[(IntPercent, IntPercent, Duration)].contramap(p =>
      (p.minPercentTime, p.minPercentTotalTime, p.totalTime)
    )

  implicit val arbIntensive: Arbitrary[Intensive] =
    Arbitrary {
      for {
        minPct    <- arbitrary[IntPercent]
        minTotPct <- arbitrary[IntPercent]
        totalTime <- arbitrary[Duration]
      } yield Intensive(minPct, minTotPct, totalTime)
    }

  implicit val cogIntensive: Cogen[Intensive] =
    Cogen[(IntPercent, IntPercent, Duration)].contramap(p =>
      (p.minPercentTime, p.minPercentTotalTime, p.totalTime)
    )

  implicit val arbProposalClass: Arbitrary[ProposalClass] =
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

  implicit val cogProposalClass: Cogen[ProposalClass] =
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

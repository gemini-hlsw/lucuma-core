// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.core.model.IntPercent
import lucuma.core.model.NonNegDuration
import lucuma.core.model.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ProposalClass { val minPercentTime: IntPercent }

object ProposalClass {

  final case class Classical(minPercentTime: IntPercent)          extends ProposalClass
  final case class DemoScience(minPercentTime: IntPercent)        extends ProposalClass
  final case class DirectorsTime(minPercentTime: IntPercent)      extends ProposalClass
  final case class Exchange(minPercentTime: IntPercent)           extends ProposalClass
  final case class FastTurnaround(minPercentTime: IntPercent)     extends ProposalClass
  final case class PoorWeather(minPercentTime: IntPercent)        extends ProposalClass
  final case class Queue(minPercentTime: IntPercent)              extends ProposalClass
  final case class SystemVerification(minPercentTime: IntPercent) extends ProposalClass

  final case class LargeProgram(
    minPercentTime:      IntPercent,
    minPercentTotalTime: IntPercent,
    totalTime:           NonNegDuration
  ) extends ProposalClass

  final case class Intensive(
    minPercentTime:      IntPercent,
    minPercentTotalTime: IntPercent,
    totalTime:           NonNegDuration
  ) extends ProposalClass

  val classical: Prism[ProposalClass, Classical]                   = GenPrism[ProposalClass, Classical]
  val demoScience: Prism[ProposalClass, DemoScience]               = GenPrism[ProposalClass, DemoScience]
  val directorsTime: Prism[ProposalClass, DirectorsTime]           = GenPrism[ProposalClass, DirectorsTime]
  val exchange: Prism[ProposalClass, Exchange]                     = GenPrism[ProposalClass, Exchange]
  val fastTurnaround: Prism[ProposalClass, FastTurnaround]         = GenPrism[ProposalClass, FastTurnaround]
  val poorWeather: Prism[ProposalClass, PoorWeather]               = GenPrism[ProposalClass, PoorWeather]
  val queue: Prism[ProposalClass, Queue]                           = GenPrism[ProposalClass, Queue]
  val systemVerification: Prism[ProposalClass, SystemVerification] =
    GenPrism[ProposalClass, SystemVerification]
  val largeProgram: Prism[ProposalClass, LargeProgram]             = GenPrism[ProposalClass, LargeProgram]
  val intensive: Prism[ProposalClass, Intensive]                   = GenPrism[ProposalClass, Intensive]

  val minPercentTime: Lens[ProposalClass, IntPercent] =
    Lens[ProposalClass, IntPercent](_.minPercentTime) { pct =>
      {
        case p @ Classical(_)          => Classical.minPercentTime.replace(pct)(p)
        case p @ DemoScience(_)        => DemoScience.minPercentTime.replace(pct)(p)
        case p @ DirectorsTime(_)      => DirectorsTime.minPercentTime.replace(pct)(p)
        case p @ Exchange(_)           => Exchange.minPercentTime.replace(pct)(p)
        case p @ FastTurnaround(_)     => FastTurnaround.minPercentTime.replace(pct)(p)
        case p @ PoorWeather(_)        => PoorWeather.minPercentTime.replace(pct)(p)
        case p @ Queue(_)              => Queue.minPercentTime.replace(pct)(p)
        case p @ SystemVerification(_) => SystemVerification.minPercentTime.replace(pct)(p)
        case p @ LargeProgram(_, _, _) => LargeProgram.minPercentTime.replace(pct)(p)
        case p @ Intensive(_, _, _)    => Intensive.minPercentTime.replace(pct)(p)
      }
    }

  val minPercentTotalTime: Optional[ProposalClass, IntPercent] =
    Optional[ProposalClass, IntPercent] {
      case LargeProgram(_, minPercentTotalTime, _) => minPercentTotalTime.some
      case Intensive(_, minPercentTotalTime, _)    => minPercentTotalTime.some
      case _                                       => none
    } { pct =>
      {
        case p @ LargeProgram(_, _, _) => LargeProgram.minPercentTotalTime.replace(pct)(p)
        case p @ Intensive(_, _, _)    => Intensive.minPercentTotalTime.replace(pct)(p)
        case p @ _                     => p
      }
    }

  val totalTime: Optional[ProposalClass, NonNegDuration] =
    Optional[ProposalClass, NonNegDuration] {
      case LargeProgram(_, _, totalTime) => totalTime.some
      case Intensive(_, _, totalTime)    => totalTime.some
      case _                             => none
    } { duration =>
      {
        case p @ LargeProgram(_, _, _) => LargeProgram.totalTime.replace(duration)(p)
        case p @ Intensive(_, _, _)    => Intensive.totalTime.replace(duration)(p)
        case p @ _                     => p
      }
    }

  object Classical {
    val minPercentTime: Lens[Classical, IntPercent] = Focus[Classical](_.minPercentTime)

    implicit val eqClassical: Eq[Classical] = Eq.by(_.minPercentTime)
  }

  object DemoScience {
    val minPercentTime: Lens[DemoScience, IntPercent] = Focus[DemoScience](_.minPercentTime)

    implicit val eqDemoScience: Eq[DemoScience] = Eq.by(_.minPercentTime)
  }

  object DirectorsTime {
    val minPercentTime: Lens[DirectorsTime, IntPercent] = Focus[DirectorsTime](_.minPercentTime)

    implicit val eqDirectorsTime: Eq[DirectorsTime] = Eq.by(_.minPercentTime)
  }

  object Exchange {
    val minPercentTime: Lens[Exchange, IntPercent] = Focus[Exchange](_.minPercentTime)

    implicit val eqExchange: Eq[Exchange] = Eq.by(_.minPercentTime)
  }

  object FastTurnaround {
    val minPercentTime: Lens[FastTurnaround, IntPercent] = Focus[FastTurnaround](_.minPercentTime)

    implicit val eqFastTurnaround: Eq[FastTurnaround] = Eq.by(_.minPercentTime)
  }

  object PoorWeather {
    val minPercentTime: Lens[PoorWeather, IntPercent] = Focus[PoorWeather](_.minPercentTime)

    implicit val eqPoorWeather: Eq[PoorWeather] = Eq.by(_.minPercentTime)
  }

  object Queue {
    val minPercentTime: Lens[Queue, IntPercent] = Focus[Queue](_.minPercentTime)

    implicit val eqQueue: Eq[Queue] = Eq.by(_.minPercentTime)
  }

  object SystemVerification {
    val minPercentTime: Lens[SystemVerification, IntPercent] =
      Focus[SystemVerification](_.minPercentTime)

    implicit val eqSystemVerification: Eq[SystemVerification] = Eq.by(_.minPercentTime)
  }

  object LargeProgram {
    val minPercentTime: Lens[LargeProgram, IntPercent]      = Focus[LargeProgram](_.minPercentTime)
    val minPercentTotalTime: Lens[LargeProgram, IntPercent] =
      Focus[LargeProgram](_.minPercentTotalTime)
    val totalTime: Lens[LargeProgram, NonNegDuration]       = Focus[LargeProgram](_.totalTime)

    implicit val eqLargeProgram: Eq[LargeProgram] =
      Eq.by(p => (p.minPercentTime, p.minPercentTotalTime, p.totalTime))
  }

  object Intensive {
    val minPercentTime: Lens[Intensive, IntPercent]      = Focus[Intensive](_.minPercentTime)
    val minPercentTotalTime: Lens[Intensive, IntPercent] = Focus[Intensive](_.minPercentTotalTime)
    val totalTime: Lens[Intensive, NonNegDuration]       = Focus[Intensive](_.totalTime)

    implicit val eqIntensive: Eq[Intensive] =
      Eq.by(p => (p.minPercentTime, p.minPercentTotalTime, p.totalTime))
  }

  implicit val eqProposalClass: Eq[ProposalClass] = Eq.instance {
    case (Classical(a1), Classical(a2))                       => a1 === a2
    case (DemoScience(a1), DemoScience(a2))                   => a1 === a2
    case (DirectorsTime(a1), DirectorsTime(a2))               => a1 === a2
    case (Exchange(a1), Exchange(a2))                         => a1 === a2
    case (FastTurnaround(a1), FastTurnaround(a2))             => a1 === a2
    case (PoorWeather(a1), PoorWeather(a2))                   => a1 === a2
    case (Queue(a1), Queue(a2))                               => a1 === a2
    case (SystemVerification(a1), SystemVerification(a2))     => a1 === a2
    case (LargeProgram(a1, b1, c1), LargeProgram(a2, b2, c2)) => a1 === a2 && b1 == b2 && c1 === c2
    case (Intensive(a1, b1, c1), Intensive(a2, b2, c2))       => a1 === a2 && b1 == b2 && c1 === c2
    case (_, _)                                               => false
  }
}

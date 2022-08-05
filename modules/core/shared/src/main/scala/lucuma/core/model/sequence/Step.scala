// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import lucuma.core.enums.Breakpoint
import lucuma.core.util.WithUid
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import org.typelevel.cats.time._

import java.time.Duration
import java.time.Instant

sealed trait Step {
  def id: Step.Id
  def instrumentConfig: DynamicConfig
  def stepConfig: StepConfig
}

object Step extends WithUid('s')

sealed trait FutureStep extends Step {
  def time: StepTime
  def breakpoint: Breakpoint
}

object FutureStep {
  final case class GmosNorth(
    id:               Step.Id,
    instrumentConfig: DynamicConfig.GmosNorth,
    stepConfig:       StepConfig,
    time:             StepTime,
    breakpoint:       Breakpoint
  ) extends FutureStep
  object GmosNorth {
    implicit val eqFutureStepGmosNorth: Eq[GmosNorth] =
      Eq.by(x => (x.id, x.instrumentConfig, x.stepConfig, x.time, x.breakpoint))

    /** @group Optics */
    val id: Lens[GmosNorth, Step.Id] =
      Focus[GmosNorth](_.id)

    /** @group Optics */
    val instrumentConfig: Lens[GmosNorth, DynamicConfig.GmosNorth] =
      Focus[GmosNorth](_.instrumentConfig)

    /** @group Optics */
    val stepConfig: Lens[GmosNorth, StepConfig] =
      Focus[GmosNorth](_.stepConfig)

    /** @group Optics */
    val time: Lens[GmosNorth, StepTime] =
      Focus[GmosNorth](_.time)

    /** @group Optics */
    val breakpoint: Lens[GmosNorth, Breakpoint] =
      Focus[GmosNorth](_.breakpoint)
  }

  final case class GmosSouth(
    id:               Step.Id,
    instrumentConfig: DynamicConfig.GmosSouth,
    stepConfig:       StepConfig,
    time:             StepTime,
    breakpoint:       Breakpoint
  ) extends FutureStep
  object GmosSouth {
    implicit val eqFutureStepGmosSouth: Eq[GmosSouth] =
      Eq.by(x => (x.id, x.instrumentConfig, x.stepConfig, x.time, x.breakpoint))

    /** @group Optics */
    val id: Lens[GmosSouth, Step.Id] =
      Focus[GmosSouth](_.id)

    /** @group Optics */
    val instrumentConfig: Lens[GmosSouth, DynamicConfig.GmosSouth] =
      Focus[GmosSouth](_.instrumentConfig)

    /** @group Optics */
    val stepConfig: Lens[GmosSouth, StepConfig] =
      Focus[GmosSouth](_.stepConfig)

    /** @group Optics */
    val time: Lens[GmosSouth, StepTime] =
      Focus[GmosSouth](_.time)

    /** @group Optics */
    val breakpoint: Lens[GmosSouth, Breakpoint] =
      Focus[GmosSouth](_.breakpoint)
  }

  implicit val eqFutureStep: Eq[FutureStep] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _, _), b @ GmosNorth(_, _, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _, _), b @ GmosSouth(_, _, _, _, _)) => a === b
    case _                                                            => false
  }

  /** @group Optics */
  val gmosNorth: Prism[FutureStep, GmosNorth] =
    GenPrism[FutureStep, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[FutureStep, GmosSouth] =
    GenPrism[FutureStep, GmosSouth]
}

sealed trait StepRecord extends Step {
  def created: Instant
  def startTime: Option[Instant]
  def endTime: Option[Instant]

  def duration: Option[Duration] =
    (startTime, endTime).mapN(Duration.between)
}

object StepRecord {
  final case class GmosNorth(
    id:               Step.Id,
    created:          Instant,
    startTime:        Option[Instant],
    endTime:          Option[Instant],
    instrumentConfig: DynamicConfig.GmosNorth,
    stepConfig:       StepConfig
  ) extends StepRecord
  object GmosNorth {
    implicit val eqStepRecordGmosNorth: Eq[GmosNorth] =
      Eq.by(x => (x.id, x.created, x.startTime, x.endTime, x.instrumentConfig, x.stepConfig))

    /** @group Optics */
    val id: Lens[GmosNorth, Step.Id] =
      Focus[GmosNorth](_.id)

    /** @group Optics */
    val created: Lens[GmosNorth, Instant] =
      Focus[GmosNorth](_.created)

    /** @group Optics */
    val startTime: Lens[GmosNorth, Option[Instant]] =
      Focus[GmosNorth](_.startTime)

    /** @group Optics */
    val endTime: Lens[GmosNorth, Option[Instant]] =
      Focus[GmosNorth](_.endTime)

    /** @group Optics */
    val instrumentConfig: Lens[GmosNorth, DynamicConfig.GmosNorth] =
      Focus[GmosNorth](_.instrumentConfig)

    /** @group Optics */
    val stepConfig: Lens[GmosNorth, StepConfig] =
      Focus[GmosNorth](_.stepConfig)
  }

  final case class GmosSouth(
    id:               Step.Id,
    created:          Instant,
    startTime:        Option[Instant],
    endTime:          Option[Instant],
    instrumentConfig: DynamicConfig.GmosSouth,
    stepConfig:       StepConfig
  ) extends StepRecord

  object GmosSouth {
    implicit val eqStepRecordGmosSouth: Eq[GmosSouth] =
      Eq.by(x => (x.id, x.created, x.startTime, x.endTime, x.instrumentConfig, x.stepConfig))

    /** @group Optics */
    val id: Lens[GmosSouth, Step.Id] =
      Focus[GmosSouth](_.id)

    /** @group Optics */
    val created: Lens[GmosSouth, Instant] =
      Focus[GmosSouth](_.created)

    /** @group Optics */
    val startTime: Lens[GmosSouth, Option[Instant]] =
      Focus[GmosSouth](_.startTime)

    /** @group Optics */
    val endTime: Lens[GmosSouth, Option[Instant]] =
      Focus[GmosSouth](_.endTime)

    /** @group Optics */
    val instrumentConfig: Lens[GmosSouth, DynamicConfig.GmosSouth] =
      Focus[GmosSouth](_.instrumentConfig)

    /** @group Optics */
    val stepConfig: Lens[GmosSouth, StepConfig] =
      Focus[GmosSouth](_.stepConfig)
  }

  implicit val eqStepRecord: Eq[StepRecord] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _, _, _), b @ GmosNorth(_, _, _, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _, _, _), b @ GmosSouth(_, _, _, _, _, _)) => a === b
    case _                                                                  => false
  }

  /** @group Optics */
  val gmosNorth: Prism[StepRecord, GmosNorth] =
    GenPrism[StepRecord, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[StepRecord, GmosSouth] =
    GenPrism[StepRecord, GmosSouth]
}

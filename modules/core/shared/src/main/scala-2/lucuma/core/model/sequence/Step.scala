// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import lucuma.core.`enum`.Breakpoint
import lucuma.core.util.WithUid
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait Step {
  val id: Step.Id
  val instrumentConfig: DynamicConfig
  val stepConfig: StepConfig
  val time: StepTime
  val breakpoint: Breakpoint
}

object Step extends WithUid('s') {
  final case class GmosNorth(
    id:               Step.Id,
    instrumentConfig: DynamicConfig.GmosNorth,
    stepConfig:       StepConfig,
    time:             StepTime,
    breakpoint:       Breakpoint
  ) extends Step
  object GmosNorth {
    implicit val eqStepGmosNorth: Eq[GmosNorth] =
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
  ) extends Step
  object GmosSouth {
    implicit val eqStepGmosSouth: Eq[GmosSouth] =
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

  implicit val eqStep: Eq[Step] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _, _), b @ GmosNorth(_, _, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _, _), b @ GmosSouth(_, _, _, _, _)) => a === b
    case _                                                            => false
  }

  /** @group Optics */
  val gmosNorth: Prism[Step, GmosNorth] =
    GenPrism[Step, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[Step, GmosSouth] =
    GenPrism[Step, GmosSouth]
}

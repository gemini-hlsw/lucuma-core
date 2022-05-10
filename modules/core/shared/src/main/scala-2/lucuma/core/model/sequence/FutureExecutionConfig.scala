// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait FutureExecutionConfig {
  val static: StaticConfig
  val acquisition: ExecutionSequence
  val science: ExecutionSequence
}

object FutureExecutionConfig {
  final case class GmosNorth(
    static:      StaticConfig.GmosNorth,
    acquisition: ExecutionSequence.GmosNorth,
    science:     ExecutionSequence.GmosNorth
  ) extends FutureExecutionConfig
  object GmosNorth {
    implicit val eqFutureExecutionConfigGmosNorth: Eq[GmosNorth] =
      Eq.by(x => (x.static, x.acquisition, x.science))

    /** @group Optics */
    val static: Lens[GmosNorth, StaticConfig.GmosNorth] =
      Focus[GmosNorth](_.static)

    /** @group Optics */
    val acquisition: Lens[GmosNorth, ExecutionSequence.GmosNorth] =
      Focus[GmosNorth](_.acquisition)

    /** @group Optics */
    val science: Lens[GmosNorth, ExecutionSequence.GmosNorth] =
      Focus[GmosNorth](_.science)
  }

  final case class GmosSouth(
    static:      StaticConfig.GmosSouth,
    acquisition: ExecutionSequence.GmosSouth,
    science:     ExecutionSequence.GmosSouth
  ) extends FutureExecutionConfig
  object GmosSouth {
    implicit val eqFutureExecutionConfigGmosSouth: Eq[GmosSouth] =
      Eq.by(x => (x.static, x.acquisition, x.science))

    /** @group Optics */
    val static: Lens[GmosSouth, StaticConfig.GmosSouth] =
      Focus[GmosSouth](_.static)

    /** @group Optics */
    val acquisition: Lens[GmosSouth, ExecutionSequence.GmosSouth] =
      Focus[GmosSouth](_.acquisition)

    /** @group Optics */
    val science: Lens[GmosSouth, ExecutionSequence.GmosSouth] =
      Focus[GmosSouth](_.science)
  }

  implicit val eqFutureExecutionConfig: Eq[FutureExecutionConfig] = Eq.instance {
    case (a @ GmosNorth(_, _, _), b @ GmosNorth(_, _, _)) => a === b
    case (a @ GmosSouth(_, _, _), b @ GmosSouth(_, _, _)) => a === b
    case _                                                => false
  }

  /** @group Optics */
  val gmosNorth: Prism[FutureExecutionConfig, GmosNorth] =
    GenPrism[FutureExecutionConfig, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[FutureExecutionConfig, GmosSouth] =
    GenPrism[FutureExecutionConfig, GmosSouth]
}

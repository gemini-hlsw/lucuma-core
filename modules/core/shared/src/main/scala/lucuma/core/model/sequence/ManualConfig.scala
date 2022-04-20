// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import org.typelevel.cats.time._

import java.time.Duration

sealed trait ManualConfig {
  val static: StaticConfig
  val setupTime: Duration
  val acquisition: Sequence
  val science: Sequence
}

object ManualConfig {
  final case class GmosNorth(
    static:      StaticConfig.GmosNorth,
    setupTime:   Duration,
    acquisition: Sequence.GmosNorth,
    science:     Sequence.GmosNorth
  ) extends ManualConfig
  object GmosNorth {
    implicit val eqManualConfigGmosNorth: Eq[GmosNorth] =
      Eq.by(x => (x.static, x.setupTime, x.acquisition, x.science))

    /** @group Optics */
    val static: Lens[GmosNorth, StaticConfig.GmosNorth] =
      Focus[GmosNorth](_.static)

    /** @group Optics */
    val setupTime: Lens[GmosNorth, Duration] =
      Focus[GmosNorth](_.setupTime)

    /** @group Optics */
    val acquisition: Lens[GmosNorth, Sequence.GmosNorth] =
      Focus[GmosNorth](_.acquisition)

    /** @group Optics */
    val science: Lens[GmosNorth, Sequence.GmosNorth] =
      Focus[GmosNorth](_.science)
  }

  final case class GmosSouth(
    static:      StaticConfig.GmosSouth,
    setupTime:   Duration,
    acquisition: Sequence.GmosSouth,
    science:     Sequence.GmosSouth
  ) extends ManualConfig
  object GmosSouth {
    implicit val eqManualConfigGmosSouth: Eq[GmosSouth] =
      Eq.by(x => (x.static, x.setupTime, x.acquisition, x.science))

    /** @group Optics */
    val static: Lens[GmosSouth, StaticConfig.GmosSouth] =
      Focus[GmosSouth](_.static)

    /** @group Optics */
    val setupTime: Lens[GmosSouth, Duration] =
      Focus[GmosSouth](_.setupTime)

    /** @group Optics */
    val acquisition: Lens[GmosSouth, Sequence.GmosSouth] =
      Focus[GmosSouth](_.acquisition)

    /** @group Optics */
    val science: Lens[GmosSouth, Sequence.GmosSouth] =
      Focus[GmosSouth](_.science)
  }

  implicit val eqManualConfig: Eq[ManualConfig] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _), b @ GmosNorth(_, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _), b @ GmosSouth(_, _, _, _)) => a === b
    case _                                                      => false
  }

  /** @group Optics */
  val gmosNorth: Prism[ManualConfig, GmosNorth] =
    GenPrism[ManualConfig, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[ManualConfig, GmosSouth] =
    GenPrism[ManualConfig, GmosSouth]
}

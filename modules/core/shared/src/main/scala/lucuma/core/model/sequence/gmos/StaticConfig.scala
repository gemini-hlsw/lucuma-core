// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait StaticConfig

object StaticConfig {

  final case class GmosNorth(
    stageMode:     GmosNorthStageMode,
    detector:      GmosNorthDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[GmosNodAndShuffle]
  ) extends StaticConfig

  object GmosNorth {
    given Eq[GmosNorth] =
      Eq.by(x => (x.stageMode, x.detector, x.mosPreImaging, x.nodAndShuffle))

    /** @group Optics */
    val stageMode: Lens[GmosNorth, GmosNorthStageMode] =
      Focus[GmosNorth](_.stageMode)

    /** @group Optics */
    val detector: Lens[GmosNorth, GmosNorthDetector] =
      Focus[GmosNorth](_.detector)

    /** @group Optics */
    val mosPreImaging: Lens[GmosNorth, MosPreImaging] =
      Focus[GmosNorth](_.mosPreImaging)

    /** @group Optics */
    val nodAndShuffle: Lens[GmosNorth, Option[GmosNodAndShuffle]] =
      Focus[GmosNorth](_.nodAndShuffle)
  }

  final case class GmosSouth(
    stageMode:     GmosSouthStageMode,
    detector:      GmosSouthDetector,
    mosPreImaging: MosPreImaging,
    nodAndShuffle: Option[GmosNodAndShuffle]
  ) extends StaticConfig

  object GmosSouth {
    given Eq[GmosSouth] =
      Eq.by(x => (x.stageMode, x.detector, x.mosPreImaging, x.nodAndShuffle))

    /** @group Optics */
    val stageMode: Lens[GmosSouth, GmosSouthStageMode] =
      Focus[GmosSouth](_.stageMode)

    /** @group Optics */
    val detector: Lens[GmosSouth, GmosSouthDetector] =
      Focus[GmosSouth](_.detector)

    /** @group Optics */
    val mosPreImaging: Lens[GmosSouth, MosPreImaging] =
      Focus[GmosSouth](_.mosPreImaging)

    /** @group Optics */
    val nodAndShuffle: Lens[GmosSouth, Option[GmosNodAndShuffle]] =
      Focus[GmosSouth](_.nodAndShuffle)
  }

  given Eq[StaticConfig] = Eq.instance {
    case (a @ GmosNorth(_, _, _, _), b @ GmosNorth(_, _, _, _)) => a === b
    case (a @ GmosSouth(_, _, _, _), b @ GmosSouth(_, _, _, _)) => a === b
    case _                                                      => false
  }

  /** @group Optics */
  val gmosNorth: Prism[StaticConfig, GmosNorth] =
    GenPrism[StaticConfig, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[StaticConfig, GmosSouth] =
    GenPrism[StaticConfig, GmosSouth]
}

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import cats.syntax.order.*
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Per-step SCORPIO configuration. Each channel is configured independently and is
 * `None` when off.
 *
 * In practice g/r and i/z share a controller, so each pair is expected to use the same settings,
 * but that is left to the sequence generator.
 */
final case class ScorpioDynamicConfig(
  g:         Option[ScorpioChannelConfig],
  r:         Option[ScorpioChannelConfig],
  i:         Option[ScorpioChannelConfig],
  z:         Option[ScorpioChannelConfig],
  y:         Option[ScorpioChannelConfig],
  j:         Option[ScorpioChannelConfig],
  h:         Option[ScorpioChannelConfig],
  ks:        Option[ScorpioChannelConfig],
  fpu:       ScorpioFpuMask,
  disperser: ScorpioDisperser
) derives Eq:

  def channels: List[ScorpioChannelConfig] =
    List(g, r, i, z, y, j, h, ks).flatten

  /**
   * Longest total exposure among the active channels.
   */
  def totalExposureTime: TimeSpan =
    channels.map(_.totalExposureTime).foldLeft(TimeSpan.Zero)(_ max _)

object ScorpioDynamicConfig:
  /** @group Optics */
  val g: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.g)

  /** @group Optics */
  val r: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.r)

  /** @group Optics */
  val i: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.i)

  /** @group Optics */
  val z: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.z)

  /** @group Optics */
  val y: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.y)

  /** @group Optics */
  val j: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.j)

  /** @group Optics */
  val h: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.h)

  /** @group Optics */
  val ks: Lens[ScorpioDynamicConfig, Option[ScorpioChannelConfig]] =
    Focus[ScorpioDynamicConfig](_.ks)

  /** @group Optics */
  val fpu: Lens[ScorpioDynamicConfig, ScorpioFpuMask] =
    Focus[ScorpioDynamicConfig](_.fpu)

  /** @group Optics */
  val disperser: Lens[ScorpioDynamicConfig, ScorpioDisperser] =
    Focus[ScorpioDynamicConfig](_.disperser)

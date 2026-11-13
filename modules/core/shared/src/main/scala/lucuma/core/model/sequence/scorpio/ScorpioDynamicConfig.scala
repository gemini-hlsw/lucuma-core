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
 * Per-step SCORPIO configuration. Each science detector is configured independently and is
 * `None` when off.
 *
 * In practice g/r and i/z share a controller, so each pair is expected to use the same settings,
 * but that is left to the sequence generator.
 */
final case class ScorpioDynamicConfig(
  g:         Option[ScorpioDetector],
  r:         Option[ScorpioDetector],
  i:         Option[ScorpioDetector],
  z:         Option[ScorpioDetector],
  y:         Option[ScorpioDetector],
  j:         Option[ScorpioDetector],
  h:         Option[ScorpioDetector],
  ks:        Option[ScorpioDetector],
  fpu:       ScorpioFpuMask,
  disperser: ScorpioDisperser
) derives Eq:

  def detectors: List[ScorpioDetector] =
    List(g, r, i, z, y, j, h, ks).flatten

  /**
   * Longest total exposure among the active detectors.
   */
  def totalExposureTime: TimeSpan =
    detectors.map(_.totalExposureTime).foldLeft(TimeSpan.Zero)(_ max _)

object ScorpioDynamicConfig:
  /** @group Optics */
  val g: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.g)

  /** @group Optics */
  val r: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.r)

  /** @group Optics */
  val i: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.i)

  /** @group Optics */
  val z: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.z)

  /** @group Optics */
  val y: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.y)

  /** @group Optics */
  val j: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.j)

  /** @group Optics */
  val h: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.h)

  /** @group Optics */
  val ks: Lens[ScorpioDynamicConfig, Option[ScorpioDetector]] =
    Focus[ScorpioDynamicConfig](_.ks)

  /** @group Optics */
  val fpu: Lens[ScorpioDynamicConfig, ScorpioFpuMask] =
    Focus[ScorpioDynamicConfig](_.fpu)

  /** @group Optics */
  val disperser: Lens[ScorpioDynamicConfig, ScorpioDisperser] =
    Focus[ScorpioDynamicConfig](_.disperser)

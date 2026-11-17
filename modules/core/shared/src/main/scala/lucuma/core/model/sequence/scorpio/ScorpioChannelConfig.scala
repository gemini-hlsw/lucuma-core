// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ScorpioVisReadoutMode
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Exposure settings for one SCORPIO channel within a step.
 */
sealed trait ScorpioChannelConfig derives Eq:
  def exposureTime: TimeSpan
  def exposureCount: PosInt

  def totalExposureTime: TimeSpan =
    exposureTime *| exposureCount.value

object ScorpioChannelConfig:

  /**
   * A VIS channel (g, r, i or z).
   */
  final case class Vis(
    exposureTime:  TimeSpan,
    exposureCount: PosInt,
    readoutMode:   ScorpioVisReadoutMode
  ) extends ScorpioChannelConfig derives Eq

  object Vis:
    /** @group Optics */
    val exposureTime: Lens[Vis, TimeSpan] =
      Focus[Vis](_.exposureTime)

    /** @group Optics */
    val exposureCount: Lens[Vis, PosInt] =
      Focus[Vis](_.exposureCount)

    /** @group Optics */
    val readoutMode: Lens[Vis, ScorpioVisReadoutMode] =
      Focus[Vis](_.readoutMode)

  /**
   * An IR channel (Y, J, H or Ks).
   */
  final case class Ir(
    exposureTime:  TimeSpan,
    exposureCount: PosInt
  ) extends ScorpioChannelConfig derives Eq

  object Ir:
    /** @group Optics */
    val exposureTime: Lens[Ir, TimeSpan] =
      Focus[Ir](_.exposureTime)

    /** @group Optics */
    val exposureCount: Lens[Ir, PosInt] =
      Focus[Ir](_.exposureCount)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Exposure settings for one SCORPIO channel within a step.
 */
final case class ScorpioChannelConfig(
  exposureTime:  TimeSpan,
  exposureCount: PosInt
) derives Eq:

  def totalExposureTime: TimeSpan =
    exposureTime *| exposureCount.value

object ScorpioChannelConfig:
  /** @group Optics */
  val exposureTime: Lens[ScorpioChannelConfig, TimeSpan] =
    Focus[ScorpioChannelConfig](_.exposureTime)

  /** @group Optics */
  val exposureCount: Lens[ScorpioChannelConfig, PosInt] =
    Focus[ScorpioChannelConfig](_.exposureCount)

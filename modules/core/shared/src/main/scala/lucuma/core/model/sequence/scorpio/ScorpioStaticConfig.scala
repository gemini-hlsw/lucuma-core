// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import lucuma.core.enums.ScorpioAdc
import lucuma.core.enums.ScorpioChannelSet
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Configuration shared by SCORPIO imaging and long-slit observations. The SVC (slit viewing
 * camera) is only used in long-slit, so its exposure time is empty for imaging.
 */
final case class ScorpioStaticConfig(
  channelSet:      ScorpioChannelSet,
  adc:             ScorpioAdc,
  svcExposureTime: Option[TimeSpan]
) derives Eq

object ScorpioStaticConfig:
  /** @group Optics */
  val channelSet: Lens[ScorpioStaticConfig, ScorpioChannelSet] =
    Focus[ScorpioStaticConfig](_.channelSet)

  /** @group Optics */
  val adc: Lens[ScorpioStaticConfig, ScorpioAdc] =
    Focus[ScorpioStaticConfig](_.adc)

  /** @group Optics */
  val svcExposureTime: Lens[ScorpioStaticConfig, Option[TimeSpan]] =
    Focus[ScorpioStaticConfig](_.svcExposureTime)

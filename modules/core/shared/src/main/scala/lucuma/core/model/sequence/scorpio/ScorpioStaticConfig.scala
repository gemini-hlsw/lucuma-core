// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import lucuma.core.enums.ScorpioAdc
import lucuma.core.enums.ScorpioArmSet
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Configuration shared by SCORPIO imaging and long-slit observations. The SVC (slit viewing
 * camera) and VIS Nod-and-Shift are only used in long-slit, so they are empty for imaging.
 *
 * ADC and SVC default to off because both are currently disabled in the hardware.
 */
final case class ScorpioStaticConfig(
  armSet:          ScorpioArmSet,
  adc:             ScorpioAdc                 = ScorpioAdc.Out,
  svcExposureTime: Option[TimeSpan]           = None,
  nodAndShift:     Option[ScorpioNodAndShift] = None
) derives Eq

object ScorpioStaticConfig:
  /** @group Optics */
  val armSet: Lens[ScorpioStaticConfig, ScorpioArmSet] =
    Focus[ScorpioStaticConfig](_.armSet)

  /** @group Optics */
  val adc: Lens[ScorpioStaticConfig, ScorpioAdc] =
    Focus[ScorpioStaticConfig](_.adc)

  /** @group Optics */
  val svcExposureTime: Lens[ScorpioStaticConfig, Option[TimeSpan]] =
    Focus[ScorpioStaticConfig](_.svcExposureTime)

  /** @group Optics */
  val nodAndShift: Lens[ScorpioStaticConfig, Option[ScorpioNodAndShift]] =
    Focus[ScorpioStaticConfig](_.nodAndShift)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.syntax.timespan.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

/**
 * SCORPIO VIS CCD readout modes, with the approximate time to read one detector in imaging
 * (central 1024 x 1024 window) and in long slit (4096 x 1024).
 *
 * The readout times are very rought estimates
 *
 * @group Enumerations
 */
enum ScorpioVisReadoutMode(
  val tag:                 String,
  val shortName:           String,
  val longName:            String,
  val imagingReadoutTime:  TimeSpan,
  val longSlitReadoutTime: TimeSpan
) derives Enumerated, Display:
  case LowNoise extends ScorpioVisReadoutMode("low_noise", "Low noise", "Low noise, 50 kHz", 5200.msTimeSpan, 20600.msTimeSpan)
  case Fast     extends ScorpioVisReadoutMode("fast",      "Fast",      "Fast, 1 MHz",       450.msTimeSpan,  1060.msTimeSpan)

object ScorpioVisReadoutMode:
  val Default: ScorpioVisReadoutMode = LowNoise

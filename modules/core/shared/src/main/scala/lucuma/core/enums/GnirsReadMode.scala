// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core
package enums

import lucuma.core.syntax.timespan.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

/**
 * Enumerated type for GNIRS Read Mode.
 * @group Enumerations (Generated)
 */
enum GnirsReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val minimumExposureTime: TimeSpan,
  val readoutTimePerCoadd: TimeSpan,
  val readNoise: Int,
  val lowNoiseReads: Int,
  val digitalAverages: Int // number of on-controller digital averages per coadd
) derives Enumerated, Display:
    case VeryBright extends GnirsReadMode("VeryBright", "Very bright", "Very Bright Acquisition or High Background", 200.msTimeSpan, 190.msTimeSpan, 155, 1, 1)
    case Bright extends GnirsReadMode("Bright", "Bright", "Bright objects", 600.msTimeSpan, 690.msTimeSpan, 30, 1, 16)
    case Faint extends GnirsReadMode("Faint", "Faint", "Faint objects", 9000.msTimeSpan, 11140.msTimeSpan, 10, 16, 16)
    case VeryFaint extends GnirsReadMode("VeryFaint", "Very faint", "Very faint objects", 18000.msTimeSpan, 22310.msTimeSpan, 7, 32, 16)

object GnirsReadMode:
  // https://app.shortcut.com/lucuma/story/8557/set-gnirs-read-mode-based-on-exposure-time
  // ATTENTION: This logic is duplicated in the DB view in the ODB. Modify it there too if it's changed here.
  def forExposureTime(t: TimeSpan): GnirsReadMode =
    val exposureSeconds: BigDecimal = t.toSeconds
    if (exposureSeconds < 0.6) GnirsReadMode.VeryBright
    else if (exposureSeconds <= 20) GnirsReadMode.Bright
    else if (exposureSeconds <= 60) GnirsReadMode.Faint
    else GnirsReadMode.VeryFaint

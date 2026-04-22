// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Read Mode.
 * @group Enumerations (Generated)
 */
enum GnirsReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int,
  val minimumExposureTime: Int,
  val readNoise: Int,
  val readNoiseLow: Int
) derives Enumerated, Display:
    case VeryBright extends GnirsReadMode("VeryBright", "Very bright", "Very Bright Acquisition or High Background", 200, 1, 155, 1)
    case Bright extends GnirsReadMode("Bright", "Bright", "Bright objects", 600, 2, 30, 1)
    case Faint extends GnirsReadMode("Faint", "Faint", "Faint objects", 3, 9000, 10, 16)
    case VeryFaint extends GnirsReadMode("VeryFaint", "Very faint", "Very faint objects", 18000, 4, 7, 32)
    
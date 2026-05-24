// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

// Similar values to GnirsReadMode but unrelated.
enum GnirsAcquisitionType(val tag: String, val shortName: String, val longName: String) derives Enumerated, Display:
  case VeryBright extends GnirsAcquisitionType("VeryBright", "Very Bright", "Very Bright Target")
  case Bright extends GnirsAcquisitionType("Bright", "Bright", "Bright Target")
  case Faint extends GnirsAcquisitionType("Faint", "Faint", "Faint Target")

object GnirsAcquisitionType:
  def forAcquisitionExposureTime(t: TimeSpan): GnirsAcquisitionType =
    val exposureSeconds: BigDecimal = t.toSeconds
    if (exposureSeconds < 1) GnirsAcquisitionType.VeryBright
    else if (exposureSeconds <= 10) GnirsAcquisitionType.Bright
    else GnirsAcquisitionType.Faint
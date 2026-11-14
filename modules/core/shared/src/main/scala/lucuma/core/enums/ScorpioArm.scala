// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * SCORPIO optical arms. Light is split by dichroics into a visible and an infrared arm,
 * each with its own detectors.
 * @group Enumerations
 */
enum ScorpioArm(val tag: String, val shortName: String, val longName: String) derives Enumerated, Display:
  case Vis extends ScorpioArm("vis", "VIS", "Visible")
  case Ir  extends ScorpioArm("ir",  "IR",  "Infrared")

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * SCORPIO atmospheric dispersion corrector state.
 * @group Enumerations
 */
enum ScorpioAdc(val tag: String, val shortName: String, val longName: String) derives Enumerated, Display:
  case Out      extends ScorpioAdc("out",      "Out",      "Retracted")
  case Static   extends ScorpioAdc("static",   "Static",   "Deployed, tracking off")
  case Tracking extends ScorpioAdc("tracking", "Tracking", "Deployed, tracking on")

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * The QA verdict for an observation as a whole.  It is calculated from the
 * QA states of the observation's science datasets unless staff override it.
 * Even when some datasets fail, the observation as a whole may pass.  There is
 * no "undefined" value: an observation without a verdict simply has none.
 */
enum ObservationQaState(val tag: String, val name: String) derives Enumerated, Display:
  case Pass   extends ObservationQaState("pass",   "Pass")
  case Usable extends ObservationQaState("usable", "Usable")
  case Fail   extends ObservationQaState("fail",   "Fail")

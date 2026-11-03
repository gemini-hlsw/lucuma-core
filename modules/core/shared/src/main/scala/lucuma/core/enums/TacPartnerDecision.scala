// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

enum TacPartnerDecision(val tag: String, val name: String) derives Enumerated, Display:
  case Accepted extends TacPartnerDecision("accepted", "Accepted")
  case Rejected extends TacPartnerDecision("rejected", "Rejected")

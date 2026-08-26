// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ProposalStatus(val tag: String, val name: String) derives Enumerated:
  case NotSubmitted extends ProposalStatus("not_submitted", "Not Submitted")
  case Submitted    extends ProposalStatus("submitted",     "Submitted")
  case Accepted     extends ProposalStatus("accepted",      "Accepted")
  case NotAccepted  extends ProposalStatus("not_accepted",  "Not Accepted")


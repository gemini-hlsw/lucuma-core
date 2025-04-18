// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/** Enumerated type for status of invitations. */
enum InvitationStatus(val tag: String) derives Enumerated:
  case Pending  extends InvitationStatus("pending")
  case Redeemed extends InvitationStatus("redeemed")
  case Declined extends InvitationStatus("declined")
  case Revoked  extends InvitationStatus("revoked")

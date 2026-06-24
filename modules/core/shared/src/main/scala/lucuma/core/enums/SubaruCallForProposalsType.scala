// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum SubaruCallForProposalsType(val tag: String) derives Enumerated:
  case Normal    extends SubaruCallForProposalsType("normal")
  case Intensive extends SubaruCallForProposalsType("intensive")

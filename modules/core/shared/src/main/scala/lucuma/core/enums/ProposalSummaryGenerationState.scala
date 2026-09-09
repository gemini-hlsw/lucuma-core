// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Where a program's proposal summary regeneration stands.
 */
enum ProposalSummaryGenerationState(val tag: String, val name: String) derives Enumerated, Display:

  /** No regeneration has been requested, or the last one finished. */
  case Idle extends ProposalSummaryGenerationState("idle", "Idle")

  /** A regeneration is queued or rendering. */
  case Generating extends ProposalSummaryGenerationState("generating", "Generating")

  /** Every render of the last regeneration stopped, and at least one failed. */
  case Failed extends ProposalSummaryGenerationState("failed", "Failed")

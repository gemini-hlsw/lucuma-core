// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Target-of-Opportunity activation, in order of increasing disruption.  Used
 * both as a proposal-level ceiling (the maximum an observation in the program
 * may declare) and as the observation's own value.
 */
enum TooActivation(val tag: String, val label: String) derives Enumerated:

  /** Not a Target of Opportunity. */
  case None extends TooActivation("none", "None")

  /** Observed whenever convenient, like any other observation. */
  case Standard extends TooActivation("standard", "Standard")

  /** Observed as soon as possible, but does not displace ongoing work. */
  case Rapid extends TooActivation("rapid", "Rapid")

  /** Observed as soon as possible, displacing ongoing work where permitted. */
  case Interrupting extends TooActivation("interrupting", "Interrupting")
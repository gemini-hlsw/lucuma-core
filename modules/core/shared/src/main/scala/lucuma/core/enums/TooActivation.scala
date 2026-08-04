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

  /**
   * The execution requirement an observation with this activation gets when it
   * states none of its own -- and, because the default acts as a floor, the
   * least restrictive requirement it can end up with.  A rapid or interrupting
   * Target of Opportunity is triggered precisely because the target will not
   * wait, so allowing it to be interrupted would defeat the purpose.
   */
  def executionRequirementDefault: ExecutionRequirement =
    this match
      case None  | Standard     => ExecutionRequirement.Unconstrained
      case Rapid | Interrupting => ExecutionRequirement.Uninterruptible

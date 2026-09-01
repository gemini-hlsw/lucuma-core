// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Target-of-Opportunity activation, in order of increasing disruption.  Used
 * both as a proposal-level ceiling (the maximum an observation in the program
 * may reach) and as the observation's own value.
 *
 * This is a declared value, not a derived one: an observation is a Target of
 * Opportunity exactly when its activation is above `None`, whatever its asterism
 * holds.  It answers what the observation may do *to others*; what may be done
 * *to it* is the other axis, [[SchedulingMode]].
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

  /** Whether this is a Target of Opportunity at all. */
  def isToo: Boolean =
    this != TooActivation.None

  /**
   * Whether an activation this disruptive obliges the observation to be
   * [[SchedulingMode.Uninterruptible]].  One that displaces other science must
   * not itself be displaceable, and one promised as soon as possible should not
   * be broken up once it starts.
   */
  def requiresUninterruptible: Boolean =
    this match
      case TooActivation.Rapid | TooActivation.Interrupting => true
      case TooActivation.None | TooActivation.Standard      => false

  /**
   * Whether this activation may be paired with `mode`.  The single rule relating
   * the two axes: `Rapid` and `Interrupting` require `Uninterruptible`, and
   * everything else is free.
   */
  def isCompatibleWith(mode: SchedulingMode): Boolean =
    !requiresUninterruptible || mode == SchedulingMode.Uninterruptible

  /**
   * Whether an observation at this activation may displace one already running
   * in `victim`.
   *
   * Only `Interrupting` displaces anything, and never something
   * `Uninterruptible` -- which by [[requiresUninterruptible]] includes every
   * `Rapid` and `Interrupting` Target of Opportunity.  So no Target of
   * Opportunity can ever preempt another, and the Scheduler never has to choose
   * between two of them mid-execution.
   */
  def canPreempt(victim: SchedulingMode): Boolean =
    this == TooActivation.Interrupting && victim.isInterruptible

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
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
 *
 * There is deliberately no level between `None` and `Rapid`.  A "standard" ToO,
 * content to be observed whenever convenient once its event arrives, would be
 * scheduled exactly like ordinary science and so is simply `None`.
 */
enum TooActivation(val tag: String, val label: String) derives Enumerated:

  /** Not a Target of Opportunity; observed whenever convenient. */
  case None extends TooActivation("none", "None")

  /** Observed as soon as possible, but does not displace ongoing work. */
  case Rapid extends TooActivation("rapid", "Rapid")

  /** Observed as soon as possible, displacing ongoing work where permitted. */
  case Interrupting extends TooActivation("interrupting", "Interrupting")

  /** Whether this is a Target of Opportunity at all. */
  def isToo: Boolean =
    this =!= TooActivation.None

  /**
   * Whether this activation obliges the observation to be
   * [[SchedulingMode.Uninterruptible]], which is true of every Target of
   * Opportunity.  One that displaces other science must not itself be
   * displaceable, and one promised as soon as possible should not be broken up
   * once it starts.
   */
  def requiresUninterruptible: Boolean =
    isToo

  /**
   * Whether this activation may be paired with `mode`.  The single rule relating
   * the two axes: a Target of Opportunity requires `Uninterruptible`, and an
   * observation that is not one may take any mode.
   */
  def isCompatibleWith(mode: SchedulingMode): Boolean =
    !requiresUninterruptible || mode === SchedulingMode.Uninterruptible

  /**
   * Whether an observation at this activation may displace one already running
   * in `victim`.
   *
   * Only `Interrupting` displaces anything, and never something
   * `Uninterruptible` -- which by [[requiresUninterruptible]] includes every
   * Target of Opportunity.  So no Target of Opportunity can ever preempt another,
   * and the Scheduler never has to choose between two of them mid-execution.
   */
  def canPreempt(victim: SchedulingMode): Boolean =
    this === TooActivation.Interrupting && victim.isInterruptible
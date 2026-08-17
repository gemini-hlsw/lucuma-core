// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * What the Scheduler is permitted to do with an observation.  The values form a
 * chain of increasing power in which each one keeps every restriction below it
 * and adds one more: `Unconstrained` grants the Scheduler every freedom, and
 * `Interrupting` is the only value that lets an observation disrupt others.
 *
 * The legal states are exactly this chain, so the combinations that make no
 * sense are unrepresentable rather than validated. Splittability subsumes
 * interruptibility -- an observation willing to be delivered across separate
 * visits is necessarily willing to be stopped between them -- and interrupting
 * subsumes uninterruptibility, because an observation aggressive enough to
 * displace others must never itself be displaced. The latter is what guarantees
 * that no Target of Opportunity can ever preempt another, and so that the
 * Scheduler never has to choose between two of them mid-execution.
 *
 * Note that `NoSplitting` may still be interrupted, and that interrupting it
 * destroys the work: it is abandoned and restarted from the beginning rather
 * than resumed as a second visit. That is precisely what distinguishes it from
 * `Uninterruptible`.
 *
 * The mode says nothing about urgency. *When* an observation must happen is a
 * timing window question.
 */
enum SchedulingMode(
  val tag:             String,
  val isSplittable:    Boolean,
  val isInterruptible: Boolean,
  val mayInterrupt:    Boolean
) derives Enumerated:

  /** Normal: the sequence may be split across visits and interrupted. */
  case Unconstrained extends SchedulingMode("unconstrained", true, true, false)

  /** The sequence may be interrupted, but not planned across multiple visits. */
  case NoSplitting extends SchedulingMode("no_splitting", false, true, false)

  /** The sequence must run start-to-finish in one uninterrupted visit. */
  case Uninterruptible extends SchedulingMode("uninterruptible", false, false, false)

  /**
   * The above, and this observation may interrupt one that is already
   * executing. Reserved to Targets of Opportunity: an observation carrying this
   * mode without an opportunity target in its asterism is invalid.
   */
  case Interrupting extends SchedulingMode("interrupting", false, false, true)

  /**
   * Whether an observation in this mode may displace one running in `victim`,
   * read straight off the ladder.
   */
  def canPreempt(victim: SchedulingMode): Boolean =
    mayInterrupt && victim.isInterruptible

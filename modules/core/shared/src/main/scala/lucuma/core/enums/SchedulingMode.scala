// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * What the Scheduler is permitted to do *to* an observation.  The values form a
 * chain of increasing restriction in which each one keeps every restriction
 * below it and adds one more, so the combinations that make no sense are
 * unrepresentable rather than validated.
 *
 * Splittability subsumes interruptibility: an observation willing to be
 * delivered across separate visits is necessarily willing to be stopped between
 * them.
 *
 * Note that `NoSplitting` may still be interrupted, and that interrupting it
 * destroys the work: it is abandoned and restarted from the beginning rather
 * than resumed as a second visit. That is precisely what distinguishes it from
 * `Uninterruptible`.
 *
 * This is one of two independent axes. What an observation may do *to others* is
 * the other, answered by [[TooActivation]]; the two are related only by the rule
 * that a `Rapid` or `Interrupting` Target of Opportunity must itself be
 * `Uninterruptible`, which [[TooActivation.isCompatibleWith]] states.
 *
 * The mode says nothing about urgency. *When* an observation must happen is a
 * timing window question.
 */
enum SchedulingMode(
  val tag:             String,
  val isSplittable:    Boolean,
  val isInterruptible: Boolean
) derives Enumerated:

  /** Normal: the sequence may be split across visits and interrupted. */
  case Unconstrained extends SchedulingMode("unconstrained", true, true)

  /** The sequence may be interrupted, but not planned across multiple visits. */
  case NoSplitting extends SchedulingMode("no_splitting", false, true)

  /** The sequence must run start-to-finish in one uninterrupted visit. */
  case Uninterruptible extends SchedulingMode("uninterruptible", false, false)

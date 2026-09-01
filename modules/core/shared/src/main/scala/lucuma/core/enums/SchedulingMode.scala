// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

import InterruptibilityOption.*
import SplittabilityOption.*

/**
 * What the Scheduler is permitted to do *to* an observation.  The values form a
 * chain of increasing restriction in which each one keeps every restriction
 * below it and adds one more.
 *
 * Note that `NoSplitting` may still be interrupted, and that interrupting it
 * destroys the work: it is abandoned and restarted from the beginning rather
 * than resumed as a second visit. That is precisely what distinguishes it from
 * `Uninterruptible`.
 *
 * This is one of two independent axes. What an observation may do *to others* is
 * the other, answered by [[TooActivation]]; the two are related only by the rule
 * that a `Rapid` or `Interrupting` Target of Opportunity must itself be
 * `Uninterruptible`.
 */
enum SchedulingMode(
  val tag:              String,
  val splittability:    SplittabilityOption,
  val interruptibility: InterruptiblityOption
) derives Enumerated:

  def isSplittable: Boolean =
    splittability === SplittabilityOption.Splittable

  def isInterruptible: Boolean =
    interruptibility === InterruptibilityOption.Interruptible

  /** Normal: the sequence may be split across visits and interrupted. */
  case Unconstrained extends SchedulingMode("unconstrained", Splittable, Interruptible)

  /** The sequence may be interrupted, but not planned across multiple visits. */
  case NoSplitting extends SchedulingMode("no_splitting", NotSplittable, Interruptible)

  /** The sequence must run start-to-finish in one uninterrupted visit. */
  case Uninterruptible extends SchedulingMode("uninterruptible", NotSplittable, NotInterruptible)
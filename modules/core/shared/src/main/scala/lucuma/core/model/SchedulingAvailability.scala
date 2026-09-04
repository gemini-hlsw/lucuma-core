// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * How available an observation is for scheduling: the total time its timing
 * windows leave it open, and the stretch that total was measured over.  Neither
 * is the length of any one window, and neither has anything to do with how long
 * the observation takes to execute.
 *
 * Both figures are measured from the later of two moments -- when the
 * observation's timing windows were last declared, and when the active period
 * began -- through to the end of that period.  Observations are usually designed
 * before the semester opens, and those are measured over the whole of it; only a
 * declaration made after it has started shortens the stretch.
 *
 * Measuring from the declaration rather than always from the start of the
 * semester is what stops time that had already elapsed from counting toward
 * `timeOpen`; carrying `timeRemainingWhenDeclared` beside it is what stops an
 * approved minimum from demanding calendar that no longer existed.
 *
 * Neither figure moves as the semester goes on.  Both are fixed by the
 * declaration and the active period, so an observation never falls out of
 * approval merely because time passed.
 */
case class SchedulingAvailability(
  timeOpen:                  TimeSpan,
  timeRemainingWhenDeclared: TimeSpan
) derives Eq:

  /**
   * Read as a minimum, does this subsume `proposed`?  It does when the proposed
   * availability is at least as much as this requires -- or all the time that
   * remained to it when it was declared, if that is less, since no observation
   * can offer calendar that had already run out.
   */
  def subsumes(proposed: SchedulingAvailability): Boolean =
    val required = timeOpen min proposed.timeRemainingWhenDeclared
    proposed.timeOpen >= required

object SchedulingAvailability:

  /** An observation that withholds nothing: open for every moment it had. */
  def unconstrained(timeRemaining: TimeSpan): SchedulingAvailability =
    SchedulingAvailability(timeRemaining, timeRemaining)

  /** A minimum that subsumes anything, which is what an unrecorded one means. */
  val Zero: SchedulingAvailability =
    SchedulingAvailability(TimeSpan.Zero, TimeSpan.Zero)

  val timeOpen: Lens[SchedulingAvailability, TimeSpan]                  = Focus[SchedulingAvailability](_.timeOpen)
  val timeRemainingWhenDeclared: Lens[SchedulingAvailability, TimeSpan] = Focus[SchedulingAvailability](_.timeRemainingWhenDeclared)

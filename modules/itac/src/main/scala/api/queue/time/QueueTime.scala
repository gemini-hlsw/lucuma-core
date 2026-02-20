// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue.time


import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.TimeAccountingCategory

object QueueTime {
  /** Number of hours in each "cycle" of 100 TimeAccountingCategory countries. */
  val Quantum = Time.hours(3.0)
  val DefaultTimeAccountingCategoryOverfillAllowance = Percent(5)
}

/** TimeAccountingCategory time plus an overfill allowance, for a paricular queue. */
final case class QueueTime(timeAccountingCategoryTime: TimeAccountingCategoryTime, val overfillAllowance: Percent) {

  def TimeAccountingCategoryQuanta: TimeAccountingCategoryTime =
    TimeAccountingCategoryTime.fromFunction(p => if (timeAccountingCategoryTime(p) == Time.Zero) Time.Zero else QueueTime.Quantum)

  def apply(p: TimeAccountingCategory): Time =
    timeAccountingCategoryTime(p)

}


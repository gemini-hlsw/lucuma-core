// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue.time


import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.IntCentiPercent
import lucuma.core.util.TimeSpan

object QueueTime {
  /** Number of hours in each "cycle" of 100 TimeAccountingCategory countries. */
  val Quantum = TimeSpan.fromHoursBounded(3.0)
  val DefaultTimeAccountingCategoryOverfillAllowance = IntCentiPercent.unsafeFromPercent(5)
}

/** TimeAccountingCategory time plus an overfill allowance, for a paricular queue. */
final case class QueueTime(timeAccountingCategoryTime: TimeAccountingCategoryTime, val overfillAllowance: IntCentiPercent) {

  def TimeAccountingCategoryQuanta: TimeAccountingCategoryTime =
    TimeAccountingCategoryTime.fromFunction(p => if (timeAccountingCategoryTime(p) == TimeSpan.Zero) TimeSpan.Zero else QueueTime.Quantum)

  def apply(p: TimeAccountingCategory): TimeSpan =
    timeAccountingCategoryTime(p)

}


// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.TimeAccountingCategory

/**
 * A proposal rejection message for proposals of TimeAccountingCategorys that have already
 * been allocated all the time that they were proportioned.
 */
object RejectTimeAccountingCategoryOverAllocation {
  val name = "TimeAccountingCategory Time Limit"

  private val fullTemplate = "%s has reached allocation limit: %s"
  private val longTemplate = "%s proposal (%.1f hr) would exceed overfill limit: %s"
  def detail(p: TimeAccountingCategory, t: Time, guaranteed: BoundedTime, all: BoundedTime): String = {
    if (guaranteed.isFull)
      fullTemplate.format(p, LogMessage.formatBoundedTime(all))
    else {
      val s = all.overbook(t) map { b => LogMessage.formatBoundedTime(b) } getOrElse ""
      longTemplate.format(p, t.toHours.value, s)
    }
  }
}

case class RejectTimeAccountingCategoryOverAllocation(prop: Proposal, guaranteed: BoundedTime, all: BoundedTime) extends RejectMessage {
  def reason: String = RejectTimeAccountingCategoryOverAllocation.name
  def detail: String = RejectTimeAccountingCategoryOverAllocation.detail(prop.ntac.TimeAccountingCategory, prop.time, guaranteed, all)
}
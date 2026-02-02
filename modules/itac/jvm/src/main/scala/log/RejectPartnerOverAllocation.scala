// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.{Proposal}
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.util.{Time, BoundedTime}

/**
 * A proposal rejection message for proposals of partners that have already
 * been allocated all the time that they were proportioned.
 */
object RejectPartnerOverAllocation {
  val name = "Partner Time Limit"

  private val fullTemplate = "%s has reached allocation limit: %s"
  private val longTemplate = "%s proposal (%.1f hr) would exceed overfill limit: %s"
  def detail(p: Partner, t: Time, guaranteed: BoundedTime, all: BoundedTime): String = {
    if (guaranteed.isFull)
      fullTemplate.format(p, LogMessage.formatBoundedTime(all))
    else {
      val s = all.overbook(t) map { b => LogMessage.formatBoundedTime(b) } getOrElse ""
      longTemplate.format(p, t.toHours.value, s)
    }
  }
}

case class RejectPartnerOverAllocation(prop: Proposal, guaranteed: BoundedTime, all: BoundedTime) extends RejectMessage {
  def reason: String = RejectPartnerOverAllocation.name
  def detail: String = RejectPartnerOverAllocation.detail(prop.ntac.partner, prop.time, guaranteed, all)
}
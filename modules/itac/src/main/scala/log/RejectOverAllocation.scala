// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time

/**
 * A proposal rejection message for proposals that are too big to fit in the
 * full queue time.
 */
object RejectOverAllocation {
  val name       = "Queue Time Limit"

  val noRemaining = "All schedulable queue time in bands 1-3 has been allocated."
  def tooBig(propTime: Time, allRemainingTime: Time) =
    "Adding %.1f hrs for proposal would require more time than remaining in the queue (%.1f hrs)".format(propTime.toHours.value, allRemainingTime.toHours.value)

  def detail(prop: Proposal, remainingGuaranteedTime: Time, allRemainingTime: Time): String =
    if (remainingGuaranteedTime <= Time.Zero)
      noRemaining
    else
      tooBig(prop.time, allRemainingTime)
}

case class RejectOverAllocation(prop: Proposal, remainingGuaranteedTime: Time, allRemainingTime: Time) extends RejectMessage {
  def reason: String = RejectOverAllocation.name
  def detail: String = RejectOverAllocation.detail(prop, remainingGuaranteedTime, allRemainingTime)
}
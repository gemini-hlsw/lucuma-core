// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import cats.syntax.all.*
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.util.TimeSpan

/**
 * A proposal rejection message for proposals that are too big to fit in the
 * full queue time.
 */
object RejectOverAllocation {
  val name       = "Queue Time Limit"

  val noRemaining = "All schedulable queue time in bands 1-3 has been allocated."
  def tooBig(propTime: TimeSpan, allRemainingTime: TimeSpan) =
    "Adding %.1f hrs for proposal would require more time than remaining in the queue (%.1f hrs)".format(propTime.toHours, allRemainingTime.toHours)

  def detail(prop: Proposal, remainingGuaranteedTime: TimeSpan, allRemainingTime: TimeSpan): String =
    if (remainingGuaranteedTime <= TimeSpan.Zero)
      noRemaining
    else
      tooBig(prop.time, allRemainingTime)
}

case class RejectOverAllocation(prop: Proposal, remainingGuaranteedTime: TimeSpan, allRemainingTime: TimeSpan) extends RejectMessage {
  def reason: String = RejectOverAllocation.name
  def detail: String = RejectOverAllocation.detail(prop, remainingGuaranteedTime, allRemainingTime)
}
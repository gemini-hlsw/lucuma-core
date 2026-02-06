// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue.time


import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time

object QueueTime {
  /** Number of hours in each "cycle" of 100 Partner countries. */
  val Quantum = Time.hours(3.0)
  val DefaultPartnerOverfillAllowance = Percent(5)
}

/** Partner time plus an overfill allowance, for a paricular queue. */
final case class QueueTime(partnerTime: PartnerTime, val overfillAllowance: Percent) {

  def partnerQuanta: PartnerTime =
    PartnerTime.fromFunction(p => if (partnerTime(p) == Time.Zero) Time.Zero else QueueTime.Quantum)

  def apply(p: Partner): Time =
    partnerTime(p)

}


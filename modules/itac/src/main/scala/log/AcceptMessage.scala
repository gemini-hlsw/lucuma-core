// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.p1.ProposalShard

/**
 * Proposal acceptance message.
 */
object AcceptMessage {
  private def detailTemplate = "TODO FIX ME %s rank %5s."
  def detail(prop: ProposalShard): String = {
    detailTemplate.format(prop.reference, "TODO") // prop.ntac.ranking.format)
  }
}

case class AcceptMessage(prop: ProposalShard) extends LogMessage with ProposalDetailMessage {
  val reason: String = "Accepted"
  val detail: String = AcceptMessage.detail(prop)
}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Proposal

/**
 * Proposal acceptance message.
 */
object AcceptMessage {
  private def detailTemplate = "%s rank %5s."
  def detail(prop: Proposal): String = {
    detailTemplate.format(prop.id.TimeAccountingCategory, prop.ntac.ranking.format)
  }
}

case class AcceptMessage(prop: Proposal) extends LogMessage with ProposalDetailMessage {
  val reason: String = "Accepted"
  val detail: String = AcceptMessage.detail(prop)
}
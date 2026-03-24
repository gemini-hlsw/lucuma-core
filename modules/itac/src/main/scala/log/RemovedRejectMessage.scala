// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Proposal

case class RemovedRejectMessage(prop: Proposal) extends RejectMessage {
  def reason: String = "Unknown."
  def detail: String = "Proposal was removed from consideration."
}


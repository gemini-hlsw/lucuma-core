// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Observation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand

/**
 * An observation rejection message for observations that would cause restricted
 * bin violations if scheduled.
 */
object RejectRestrictedBin extends TimeBinMessageFormatter {
  val name = "Restricted Bin Limit"

  private val reasonTemplate = "%s: %s"
  def reason(binName: String): String = reasonTemplate.format(name, binName)
}

final case class RejectRestrictedBin(prop: Proposal, obs: Observation, band: ScienceBand, name: String, cur: Time, max: Time) extends RejectMessage {
  def reason: String = RejectRestrictedBin.reason(name)
  def detail: String = RejectRestrictedBin.detail(prop, obs, band, cur, max)
}
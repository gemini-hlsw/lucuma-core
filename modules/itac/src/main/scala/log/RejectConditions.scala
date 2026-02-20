// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand

/**
 * An observation rejection message for observing conditions restrictions
 * violations.
 */
object RejectConditions extends TimeBinMessageFormatter {
  val name = "Conditions Limit"

  private val detailTemplate = "%s %s"
  override def detail(prop: Proposal, obs: ItacObservation, band: ScienceBand, cur: Time, max: Time): String =
    detailTemplate.format(obs.constraintSet, super.detail(prop, obs, band, cur, max))
}

final case class RejectConditions(prop: Proposal, obs: ItacObservation, band: ScienceBand, cur: Time, max: Time) extends ObsRejectMessage {
  def reason: String = RejectConditions.name
  def detail: String = RejectConditions.detail(prop, obs, band, cur, max)
}

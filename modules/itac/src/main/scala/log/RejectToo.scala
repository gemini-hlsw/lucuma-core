// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand

object RejectToo {
  val name = "ToO Remaining Time"

  private val detailTemplate = "ToO observation of %.2f hours with conditions %s.  Remaining time %.2f hours."
  def detail(prop: Proposal, obs: ItacObservation, band: ScienceBand, remaining: Time): String = {
    val obsTime = prop.relativeObsTime(obs, band).toHours.value
    val remTime = remaining.toHours.value
    detailTemplate.format(obsTime, obs.constraintSet, remTime)
  }

}

final case class RejectToo(prop: Proposal, obs: ItacObservation, band: ScienceBand, remaining: Time) extends ObsRejectMessage {
  def reason: String = RejectToo.name
  def detail: String = RejectToo.detail(prop, obs, band, remaining)
}

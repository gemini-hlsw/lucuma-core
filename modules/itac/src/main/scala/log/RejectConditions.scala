// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.ProposalShard
import lucuma.core.enums.ScienceBand
import lucuma.core.util.TimeSpan

/**
 * An observation rejection message for observing conditions restrictions
 * violations.
 */
object RejectConditions extends TimeBinMessageFormatter {
  val name = "Conditions Limit"


  // imageQuality:    ImageQuality.Preset,
  // cloudExtinction: CloudExtinction.Preset,
  // skyBackground:   SkyBackground,
  // waterVapor:      WaterVapor,
  // elevationRange:  ElevationRange
  private val detailTemplate = "%s %s %s %s %s"
  override def detail(prop: ProposalShard, obs: ItacObservation, band: ScienceBand, cur: TimeSpan, max: TimeSpan): String =
    detailTemplate.format(
      obs.constraintSet.imageQuality,
      obs.constraintSet.cloudExtinction,
      obs.constraintSet.skyBackground,
      obs.constraintSet.waterVapor,
      super.detail(prop, obs, band, cur, max)
    )
}

final case class RejectConditions(prop: ProposalShard, obs: ItacObservation, band: ScienceBand, cur: TimeSpan, max: TimeSpan) extends ObsRejectMessage {
  def reason: String = RejectConditions.name
  def detail: String = RejectConditions.detail(prop, obs, band, cur, max)
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Observation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand

object RejectTarget extends TimeBinMessageFormatter {
  trait RaDecType
  case object Ra extends RaDecType {
    override def toString = "RA"
  }
  case object Dec extends RaDecType {
    override def toString = "Dec"
  }

  private val reasonTemplate = "%s Limit"
  def reason(raDecType: RaDecType): String = reasonTemplate.format(raDecType)
}

final case class RejectTarget(prop: Proposal, obs: Observation, band: ScienceBand, raDecType: RejectTarget.RaDecType, cur: Time, max: Time) extends ObsRejectMessage {
  def reason: String = RejectTarget.reason(raDecType)
  def detail: String = RejectTarget.detail(prop, obs, band, cur, max)
}

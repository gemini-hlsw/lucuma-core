// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand
import lucuma.core.util.TimeSpan

trait TimeBinMessageFormatter {
  private val binStatusTemplate = "Bin %.1f%% full (%.2f / %.2f hrs)"
  def binStatus(cur: TimeSpan, max: TimeSpan): String = {
    val curHrs = cur.toHours
    val maxHrs = max.toHours
    val perc   = if (maxHrs.abs < 0.0001) 100.0 else curHrs/maxHrs * 100
    binStatusTemplate.format(perc, curHrs, maxHrs)
  }

  private val obsInfoTemplate   = "%.2f hrs at %s(%.3f hr, %.1f deg)"
  def obsInfo(prop: Proposal, obs: ItacObservation, band: ScienceBand): String = {
    val obsTime = prop.relativeObsTime(obs, band)
    val target  = obs.itacTarget
    val targetName = target.id.toString
    obsInfoTemplate.format(obsTime.toHours, targetName, target.ra.toHourAngle.toDoubleHours, target.dec.toAngle.toDoubleDegrees)
  }

  private val detailTemplate    = "%s. Reject %s."
  def detail(prop: Proposal, obs: ItacObservation, band: ScienceBand, cur: TimeSpan, max: TimeSpan): String = {
    val statusMsg  = binStatus(cur, max)
    val obsInfoMsg = obsInfo(prop, obs, band)
    detailTemplate.format(statusMsg, obsInfoMsg)
  }
}
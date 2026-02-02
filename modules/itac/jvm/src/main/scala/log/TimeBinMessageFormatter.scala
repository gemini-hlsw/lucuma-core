// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Observation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.p1.QueueBand
import edu.gemini.tac.qengine.util.Time

trait TimeBinMessageFormatter {
  private val binStatusTemplate = "Bin %.1f%% full (%.2f / %.2f hrs)"
  def binStatus(cur: Time, max: Time): String = {
    val curHrs = cur.toHours.value
    val maxHrs = max.toHours.value
    val perc   = if (maxHrs.abs < 0.0001) 100.0 else curHrs/maxHrs * 100
    binStatusTemplate.format(perc, curHrs, maxHrs)
  }

  private val obsInfoTemplate   = "%.2f hrs at %s(%.3f hr, %.1f deg)"
  def obsInfo(prop: Proposal, obs: Observation, band: QueueBand): String = {
    val obsTime = prop.relativeObsTime(obs, band)
    val target  = obs.target
    val targetName = target.name.map(n => "'%s' ".format(n)).getOrElse("")
    obsInfoTemplate.format(obsTime.toHours.value, targetName, target.ra.toHr.mag, target.dec.toDeg.mag)
  }

  private val detailTemplate    = "%s. Reject %s."
  def detail(prop: Proposal, obs: Observation, band: QueueBand, cur: Time, max: Time): String = {
    val statusMsg  = binStatus(cur, max)
    val obsInfoMsg = obsInfo(prop, obs, band)
    detailTemplate.format(statusMsg, obsInfoMsg)
  }
}
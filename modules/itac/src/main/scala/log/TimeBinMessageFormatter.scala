// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.ProposalShard
import lucuma.core.enums.ScienceBand
import lucuma.core.util.TimeSpan
import org.typelevel.scalaccompat.annotation.nowarn

trait TimeBinMessageFormatter {
  private val binStatusTemplate = "Bin %.1f%% full (%.2f / %.2f hrs)"
  def binStatus(cur: TimeSpan, max: TimeSpan): String = {
    val curHrs = cur.toHours
    val maxHrs = max.toHours
    val perc   = if (maxHrs.abs < 0.0001) 100.0 else curHrs/maxHrs * 100
    binStatusTemplate.format(perc, curHrs, maxHrs)
  }

  @nowarn
  def obsInfo(prop: ProposalShard, obs: ItacObservation, band: ScienceBand): String = {
    val obsTime = obs.time.toHours
    val target  = obs.itacTarget
    val targetName = target.id.toString
    f"$obsTime%.2f hrs at $targetName(${target.ra.toHourAngle.toDoubleHours}%.3f hr, ${target.dec.toAngle.toDoubleDegrees}%.1f deg)"
  }

  private val detailTemplate    = "%s. Reject %s."
  def detail(prop: ProposalShard, obs: ItacObservation, band: ScienceBand, cur: TimeSpan, max: TimeSpan): String = {
    val statusMsg  = binStatus(cur, max)
    val obsInfoMsg = obsInfo(prop, obs, band)
    detailTemplate.format(statusMsg, obsInfoMsg)
  }
}
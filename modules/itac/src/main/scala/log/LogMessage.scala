// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.util.BoundedTime

import java.text.SimpleDateFormat
import java.util.Date
import java.util.UUID

trait LogMessage {
  val prop: Proposal
  val timestamp = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ").format(new Date())
  val id = UUID.randomUUID()
}

object LogMessage {
  var _counter = 0
  def counter = synchronized {
    val oldCounter = _counter
    _counter += 1
    oldCounter
  }
  private def timeTemplate = "%4.1f%% (%.1f / %.1f)"
  def formatBoundedTime(bt: BoundedTime): String = {
    val usedH = bt.used.toHours.value
    val maxH  = bt.limit.toHours.value
    val perc  = bt.fillPercent
    timeTemplate.format(perc, usedH, maxH)
  }
}
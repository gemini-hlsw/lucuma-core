// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

import edu.gemini.tac.qengine.p1.Observation
import edu.gemini.tac.qengine.p1.QueueBand

/**
 * A proposal rejection that occurs because a particular observation in the
 * proposal was rejected.  Identifies the observation in question and provides
 * some information about the percent of observations (and time) that were
 * successfully merged prior to the the rejection.
 */
trait ObsRejectMessage extends RejectMessage {
  def obs: Observation

  def percentObsMerged(b: QueueBand): Int = {
    val l = prop.obsListFor(b)
    ((l.indexOf(obs) / l.size.toDouble) * 100).round.toInt
  }

  def percentTimeMerged(b: QueueBand): Int = {
    val totalTime = prop.obsListFor(b).foldLeft(0L)(_ + _.time.ms)
    ((obs.time.ms / totalTime.toDouble) * 100).round.toInt
  }
}
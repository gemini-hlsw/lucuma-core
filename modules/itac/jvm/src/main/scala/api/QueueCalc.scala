// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api

import edu.gemini.tac.qengine.api.queue.ProposalQueue
import edu.gemini.tac.qengine.log.ProposalLog
import edu.gemini.tac.qengine.ctx.Context
import edu.gemini.tac.qengine.p1.QueueBand
import edu.gemini.tac.qengine.p1.Proposal

trait BucketsAllocation {
  def raTablesANSI: String
}

/**
 * A queue calculation result.  This is a combination of a ProposalQueue and
 * a ProposalLog.  The queue contains the selected proposals and statistics
 * while the log records what happened to the proposals that were not selected.
 */
trait QueueCalc {
  val context: Context
  def queue(band: QueueBand): ProposalQueue
  val proposalLog: ProposalLog
  val bucketsAllocation: BucketsAllocation
  def toList: List[Proposal] = QueueBand.values.flatMap(queue(_).toList)
}


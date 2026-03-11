// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api

import edu.gemini.tac.qengine.api.queue.ProposalQueue
import edu.gemini.tac.qengine.log.ProposalLog
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand
import lucuma.core.util.Enumerated
import lucuma.core.model.Semester
import lucuma.core.enums.Site

trait BucketsAllocation {
  def raTablesANSI: String
}

/**
 * A queue calculation result.  This is a combination of a ProposalQueue and
 * a ProposalLog.  The queue contains the selected proposals and statistics
 * while the log records what happened to the proposals that were not selected.
 */
trait QueueCalc {
  def site: Site
  def semester: Semester
  def queue(band: ScienceBand): ProposalQueue
  def proposalLog: ProposalLog
  def bucketsAllocation: BucketsAllocation
  def toList: List[Proposal] = Enumerated[ScienceBand].all.flatMap(queue(_).toList)
}


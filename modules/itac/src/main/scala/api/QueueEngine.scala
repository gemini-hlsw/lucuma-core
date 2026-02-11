// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api

import edu.gemini.tac.qengine.api.config.QueueEngineConfig
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand

/**
 * The queue generation engine.
 */
trait QueueEngine {
  def calc(
    bandedProposals: ScienceBand => List[Proposal],
    queueTimes:   ScienceBand => QueueTime,
    config: QueueEngineConfig,
    removed: List[Proposal] = Nil
  ): QueueCalc
}
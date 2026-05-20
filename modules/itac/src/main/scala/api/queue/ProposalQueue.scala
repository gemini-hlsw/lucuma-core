// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue

import cats.syntax.all.*
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.BoundedTime
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.TimeSpan


// Wait, do we really need to care about bands at all here? We need to know whether
// we're overfull in RA and conditions buckets, which we fill in round-robin quanta by
// by band and rank, backtracking if we can't completely allocate an observation. The 
// problem is that we can't necessarily complete an observation until we look at all
// bands.

// We want to use up the worst-band time first because its usage is the most constrained
// (we can't use it for best conditions or for ToOs). But we want to schedule "band 1 programs"
// first because they're the most important. So let's rely on the ranking. We go by ranking,
// round-robin by partner, using up the least-constrained time first.

trait ProposalQueue {

  def band: ScienceBand

  def queueTime: QueueTime // this us unused time

  def usedTime: TimeSpan =
    toList.foldMap(_.allocation.duration)

  def usedTime(category: TimeAccountingCategory): TimeSpan =
    toList
      .map(_.allocation)
      .filter(_.category === category)
      .foldMap(_.duration)

  def remainingTime(timeAccountingCategory: TimeAccountingCategory): TimeSpan =
    queueTime(timeAccountingCategory) -| usedTime(timeAccountingCategory)

  def bounds(p: TimeAccountingCategory): BoundedTime =
    BoundedTime(queueTime(p), usedTime(p))

  def toList: List[ProposalShard]

}
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

/** A queue for a single band. */
trait ProposalQueue {

  def band: ScienceBand

  def queueTime: QueueTime

  def usedTime: TimeSpan =
    toList.foldMap(_.time)

  def usedTime(p: TimeAccountingCategory): TimeSpan =
    toList.filter(_.ntac.category === p).foldMap(_.time)

  def remainingTime(TimeAccountingCategory: TimeAccountingCategory): TimeSpan =
    queueTime(TimeAccountingCategory) -| usedTime(TimeAccountingCategory)

  def bounds(p: TimeAccountingCategory): BoundedTime =
    BoundedTime(queueTime(p), usedTime(p))

  def toList: List[Proposal]

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.queue

import cats.syntax.all.*
import edu.gemini.tac.qengine.api.queue.ProposalQueue
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.p1.*

/**
 * ProposalQueueBuilder is used to construct the Band 1, 2, and 3 part of the
 * proposal queue.  In other words, for the part of the queue that is determined
 * by the relative time occupied by previous proposals in the queue.  It tracks
 * the current state of the queue computation including the ordered proposals,
 * time used up to the current point, and the queue band.  Once the queue
 * creation algorithm runs to completion the bandedQueue method may be used to
 * extract the queue for each of the 3 queue bands.
 */

final case class ProposalQueueBuilder(
  queueTime: QueueTime,
  band:      QueueBand,
  proposals: List[Proposal] = Nil
) extends ProposalQueue {

  /**
   * Adds the given proposal to the queue and returns a new ProposalQueue
   * reflecting the change.
   */
  def :+(prop: Proposal): ProposalQueueBuilder =
    copy(proposals = prop :: proposals)

  /**
   * Adds all the proposals to the queue in the traversal order.
   */
  def ++(props: Iterable[Proposal]): ProposalQueueBuilder =
    props.foldLeft(this) {
      (q, p) => q :+ p
    }

  def toList: List[Proposal] = proposals.reverse

  val bandedQueue: Map[QueueBand,List[Proposal]] =
    QueueBand.values.foldMap(b => Map(b -> List.empty[Proposal])) ++
    Map(band -> proposals)

}
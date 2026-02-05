// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage

object CompositeResource {
  def compositeReserve[A <: Resource{type T=A}, B <: Resource{type T=B}](block: Block, queue: ProposalQueueBuilder, a: A, b: B): RejectMessage Either (A, B) =
    for {
      newA <- a.reserve(block, queue)
      newB <- b.reserve(block, queue)
    } yield (newA, newB)
}

/**
 * A composite reservation combines two TimeReservation implementations of
 * possibly different types into a single TimeReservation.  It will attempt to
 * reserve the time for a block in each of its contained reservations.  If
 * either rejects the block, then the composite rejects the block.  If both
 * accept the block, an updated composite is returned.
 */
class CompositeResource[A <: Resource{type T=A}, B <: Resource{type T=B}](val _1: A, val _2: B) extends Resource {
  type T = CompositeResource[A, B]

  def this(tup: (A, B)) = this(tup._1, tup._2)

  def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either CompositeResource[A, B] =
    CompositeResource.compositeReserve(block, queue, _1, _2) map {
      tup => new CompositeResource(tup)
    }

}
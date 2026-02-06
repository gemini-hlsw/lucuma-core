// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage
import edu.gemini.tac.qengine.log.RejectRestrictedBin
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time

object TimeResource {
  def apply(bin: TimeRestriction[Percent], time: Time): TimeResource =
    new TimeResource(bin.map(percent => BoundedTime(time * percent)))

  def apply(bin: TimeRestriction[Time]): TimeResource =
    new TimeResource(bin.map(time => BoundedTime(time)))

  def reserveAll(block: Block, queue: ProposalQueueBuilder, lst: List[TimeResource]): RejectMessage Either List[TimeResource] =
    Resource.reserveAll(block, queue, lst)
}

/**
 * A TimeReservation implementation for a RestrictedBin.  Handles matching
 * blocks to the the restricted bin predicate and updating the time if
 * necessary.
 */
final class TimeResource(val bin: TimeRestriction[BoundedTime]) extends Resource {
  type T = TimeResource

  def limit: Time = bin.value.limit
  def remaining: Time = bin.value.remaining
  def isFull: Boolean = bin.value.isFull

  override def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either TimeResource =
    if (!bin.matches(block.prop, block.obs, queue.band))
      Right(this)  // didn't match so return the same reservation object
    else
      bin.value.reserve(block.time) match {
        case Some(bt) if bt.remaining == remaining => Right(this)  // no time requested
        case Some(bt) => Right(new TimeResource(bin.updated(bt))) // update bounded time
        case _ => Left(new RejectRestrictedBin(block.prop, block.obs, queue.band, bin.name, bin.value.used, bin.value.limit))
      }

}
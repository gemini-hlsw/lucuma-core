// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage
import edu.gemini.tac.qengine.log.RejectTimeAccountingCategoryOverAllocation
import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Time
import lucuma.core.model.ConstraintSet
import org.slf4j.LoggerFactory

final case class SemesterResource(
    ra:   RightAscensionMapResource,
    time: List[TimeRestriction[BoundedTime]],
) extends Resource {
  private val LOGGER = LoggerFactory.getLogger("edu.gemini.itac")
  type T = SemesterResource

  private def reserveAll(block: Block, queue: ProposalQueueBuilder): RejectMessage Either SemesterResource =
    for {
      newRa   <- ra.reserve(block, queue)
      newTime <- Resource2.reserveAll(block, queue, time)
    } yield new SemesterResource(newRa, newTime)

  // Determines whether the TimeAccountingCategory is already over allocated.
  private def TimeAccountingCategoryAlreadyOverallocated(block: Block, queue: ProposalQueueBuilder): Boolean = {
    LOGGER.debug(f"Remaining time for ${block.prop.ntac.TimeAccountingCategory} is ${queue.remainingTime(block.prop.ntac.TimeAccountingCategory).toHours.value}%5.1f")
    queue.remainingTime(block.prop.ntac.TimeAccountingCategory) <= Time.Zero
  }

  // Determines whether including the indicated proposal will overallocate the
  // TimeAccountingCategory past the limit and allowance.
  private def TimeAccountingCategoryWouldBeOverallocated(block: Block, queue: ProposalQueueBuilder): Boolean = {
    val perc      = queue.queueTime.overfillAllowance
    val TimeAccountingCategory   = block.prop.ntac.TimeAccountingCategory
    val used      = queue.usedTime(TimeAccountingCategory)
    val softLimit = queue.queueTime(TimeAccountingCategory)
    val allowance = softLimit * perc // overfill is per category (B1_2 and B3 are what we're using)
    val hardlimit = softLimit + allowance
    val ret = (used + block.prop.time) >= hardlimit
    // println(f"==> used ${used.toHours.value}%5.1f, available = ${softLimit.toHours.value}%5.1f, overfill = ${perc.value.toDouble}%5.1f, percentage = ${(used.toHours.value / softLimit.toHours.value) * 100.0}%5.1f, prop = ${block.prop.ntac.reference}, award = ${block.prop.time.toHours.value}%5.1f, TimeAccountingCategoryWouldBeOverallocated = ${ret}")
    ret
  }

  private def TimeAccountingCategoryOverallocated(block: Block, queue: ProposalQueueBuilder): Boolean =
    TimeAccountingCategoryAlreadyOverallocated(block, queue) || TimeAccountingCategoryWouldBeOverallocated(block, queue)

  def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either SemesterResource = {
    // Check that we haven't over allocated this TimeAccountingCategory.  If so, rejected.
    // Otherwise, try to reserve the time.

    // We only need to check for overallocation at the start of the block --
    // the queue.remainingTime for the TimeAccountingCategory won't be updated until another
    // proposal is added so there is no need to check with every block that is
    // considered.
    if (block.isStart && TimeAccountingCategoryOverallocated(block, queue)) {
      val p = block.prop.ntac.TimeAccountingCategory
      LOGGER.debug("Rejected due to TimeAccountingCategory overallocation")
      Left(RejectTimeAccountingCategoryOverAllocation(block.prop, queue.bounds(p), queue.bounds(p)))
    } else {
      LOGGER.debug("Block OK")
      if(block.isFinal){
        LOGGER.debug("Block is final; proposal will be accepted")
      }
      reserveAll(block, queue)
    }
  }

  def reserveAvailable(time: Time, target: Target, conds: ConstraintSet): (SemesterResource, Time) = {
    val (newRa, rem) = ra.reserveAvailable(time, target, conds)
    (new SemesterResource(newRa, this.time), rem)
  }


}
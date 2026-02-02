// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import block.{Block, BlockIterator}
import edu.gemini.tac.qengine.impl.resource.SemesterResource
import edu.gemini.tac.qengine.log.{AcceptMessage, RejectMessage}
import queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.p1.{Observation, Proposal}
import org.slf4j.LoggerFactory

/**
 * QueueFrame represents the state of the queue generation process at a
 * particular step of the block iterator.  QueueFrames are pushed onto a "stack"
 * until a problem with a proposal is encountered, at which point all frames
 * from the point at which the rejected proposal was introduced are removed and
 * the proposal is skipped.
 */
final class QueueFrame(val queue: ProposalQueueBuilder, val iter: BlockIterator, val res: SemesterResource) {
  private val LOGGER = LoggerFactory.getLogger("edu.gemini.itac")
  private val applicationLogger = LoggerFactory.getLogger("edu.gemini.itac")

  val lName = LOGGER.getName

  case class Next(frame: QueueFrame, accept: Option[AcceptMessage])

  def isStartOf(prop: Proposal): Boolean = iter.isStartOf(prop)

  def skip(activeList : Proposal => List[Observation]): QueueFrame = new QueueFrame(queue, iter.skip(activeList), res)

  def hasNext: Boolean = iter.hasNext

  private def updated(block: Block): (ProposalQueueBuilder, Option[AcceptMessage]) =
    if (block.isFinal) {
      // There will be no more blocks for this proposal, so accept it.
      val prop     = block.prop
      val newQueue = queue :+ prop
      applicationLogger.trace("accept(): " + block.toString)
      (newQueue, Some(AcceptMessage(prop)))
    } else
      // More blocks for this proposal so we can't accept it yet.
      (queue, None)

  private def logBlock(block : Block) = {
    val msg = "Block of time " + block.time.toHours + " proposed for Proposal[" +block.prop.id + "] w observation time=" + block.obs.time.toHours.toString + "" +
      " Proposal Awarded [" + block.prop.ntac.awardedTime.toHours.toString + "] by " + block.prop.ntac.partner.id + "]"
    LOGGER.debug(msg)
    //applicationLogger.log(Level.trace, "next():" + block.toString);
  }

  def next(activeList : Proposal=>List[Observation]): RejectMessage Either Next = {
    val (block, newIter) = iter.next(activeList)
    logBlock(block)
    res.reserve(block, queue) map {
      r => val (updatedQueue, accept) = updated(block)
           Next(new QueueFrame(updatedQueue, newIter, r), accept)
    }
  }

}

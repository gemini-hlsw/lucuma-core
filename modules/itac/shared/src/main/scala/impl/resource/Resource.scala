// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectMessage
import annotation.tailrec
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder

/**
 * A trait that represents an interface provided on a time bounded resource, be
 * it the number of hours for a particular RA and Dec, for a particular set of
 * observing conditions, or some combination of other lower-level resources.
 */
trait Resource {

  /**
   * Resource implementations must define the exact type that is returned by
   * the reserve method, which must be a Resource subclass.  This is intended
   * to allow the implementation to return a new instance of the same class to
   * the caller without requiring a cast.
   */
  type T <: Resource

  /**
   * Reserves the amount of time indicated by the block, if possible.  Returns
   * an updated resource that incorporates the time if possible; otherwise
   * a RejectMessage.
   *
   * ProposalQueue contains the state of the queue at the moment that we try
   * to reserve the time block.  This is necessary in some cases to determine
   * how to categorize the time for example.
   */
  def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either T
}

object Resource {

  @tailrec
  private def reserveAll[A <: Resource{type T=A}](b: Block, q: ProposalQueueBuilder, inList: List[A], outList: List[A]): RejectMessage Either List[A] =
    inList match {
      case Nil => Right(outList.reverse)  // reverse the list to maintain the original order -- really only important for testing?
      case headA :: _ => headA.reserve(b, q) match {
        case Left(msg) => Left(msg)
        case Right(newA) => reserveAll(b, q, inList.tail, newA :: outList)
      }
    }

  /**
   * A utility method that is used to apply the Resource.reserve method
   * to a list of the same kind of Resource.  If all elements except the
   * block, then an updated list is returned.  Otherwise, the first element that
   * rejects the block stops the computation and returns a RejectMessage
   */
  def reserveAll[A <: Resource{type T=A}](b: Block, q: ProposalQueueBuilder, lst: List[A]): RejectMessage Either List[A] =
    reserveAll(b, q, lst, Nil)
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.ConditionsBin
import edu.gemini.tac.qengine.api.config.ConditionsBinGroup
import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.*
import edu.gemini.tac.qengine.p1.ObservingConditions
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time

import annotation.tailrec

object ConditionsResourceGroup {

  // Absorbs the time into the BoundedTime values associated with the given
  // "in" list of observing conditions categories, returning a list of updated
  // bins and any remaining time
  @tailrec private def reserveAvailable(t: Time, in: List[ConditionsBin[BoundedTime]], out: List[ConditionsBin[BoundedTime]]): (List[ConditionsBin[BoundedTime]], Time) =
    if (t.isZero || in.isEmpty)
      (out, t)
    else {
      val (bt, spill) = in.head.binValue.reserveAvailable(t)
      reserveAvailable(spill, in.tail, in.head.updated(bt) :: out)
    }

  /**
   * Constructs with the total time to spread across the observing conditions
   * bins and the set of bins to use.
   */
  def apply(t: Time, g: ConditionsBinGroup[Percent]) = {
    // Creates a map from ConditionsBin.Category to BoundedTime initialized with
    // time values according to the relative percentage of the matching
    // conditions bin.
    new ConditionsResourceGroup(g.map(perc => BoundedTime(t * perc)))
  }
}

/**
 * A time reservation used to keep up with the time used at the various
 * observing conditions categories.  Handles spilling the time across better
 * categories when necessary and possible.
 */
final class ConditionsResourceGroup private (val bins: ConditionsBinGroup[BoundedTime]) extends Resource {
  type T = ConditionsResourceGroup

  private def sum(c: ObservingConditions, f: (BoundedTime => Time)): Time = {
    val cats = bins.searchPath(c)
    cats.foldLeft(Time.Minutes.zero)((t: Time, cat: Cat) => t + f(bins(cat)))
  }

  def limit(c: ObservingConditions): Time = sum(c, _.limit)
  def remaining(c: ObservingConditions): Time = sum(c, _.remaining)
  def isFull(c: ObservingConditions): Boolean = remaining(c).isZero

  private def conds(block: Block): ObservingConditions =
    block.obs.conditions


  /**
   * Reserves the time for the given observation into the conditions bins
   * if possible.  Returns an updated ConditionsReservation containing the
   * observation's time, or else an error.
   */
  override def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either ConditionsResourceGroup = {
    val c = conds(block)
    reserveAvailable(block.time, c) match {
      case (newGrp, t) if t.isZero => Right(newGrp)
      case _                       => Left(rejectConditions(block.prop, block.obs, queue.band, sum(c, _.used), sum(c, _.limit)))
    }
  }

  /**
   * Reserves up-to the given amount of time associated with the given set of
   * conditions.  If more time is specified than available, the bins associated
   * with these conditions will be filled and any remaining time will be
   * returned.   Returns a new conditions resource group with the reserved time
   * along with any remaining time that could not be reserved.
   */
  def reserveAvailable(time: Time, conds: ObservingConditions): (ConditionsResourceGroup, Time) = {
    val (updatedBins, rem) = ConditionsResourceGroup.reserveAvailable(time, bins.searchBins(conds), Nil)
    (new ConditionsResourceGroup(bins.updated(updatedBins)), rem)
  }

}

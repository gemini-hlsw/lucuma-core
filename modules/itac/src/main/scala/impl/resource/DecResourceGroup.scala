// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.DecBin
import edu.gemini.tac.qengine.api.config.DecBinGroup
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.block.TooBlocks
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage
import edu.gemini.tac.qengine.log.RejectTarget
import edu.gemini.tac.qengine.p1.QueueBand
import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.p1.Too
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.math.Declination

object DecResourceGroup {
  def apply(t: Time, bins: DecBinGroup[Percent]): DecResourceGroup = {
    new DecResourceGroup(bins.map(perc => BoundedTime(t * perc)))
  }

  def tooBlocks(block: Block, grp: DecBinGroup[BoundedTime]): Option[Seq[Block]] =
    TooBlocks[DecBin[BoundedTime]](block, grp.bins, _.binValue.remaining)
}

/**
 * A time reservation used to keep up with the time used at various dec
 * ranges.
 */
final case class DecResourceGroup(val bins: DecBinGroup[BoundedTime]) extends Resource {
  type T = DecResourceGroup

  private def lookup(dec: Declination, f: BoundedTime => Time): Time =
    bins.get(dec).map(bin => f(bin.binValue)).getOrElse(Time.Zero)

  def limit(dec: Declination): Time      = lookup(dec, _.limit)
  def limit(t: Target): Time       = limit(t.dec)

  def remaining(dec: Declination): Time  = lookup(dec, _.remaining)
  def remaining(t: Target): Time   = remaining(t.dec)

  def isFull(dec: Declination): Boolean  = remaining(dec).isZero
  def isFull(t: Target): Boolean   = isFull(t.dec)

  private def reserveNormal(block: Block, band: QueueBand): RejectMessage Either DecResourceGroup = {
    val dec = block.obs.target.dec
    bins.updated(dec, _.reserve(block.time)) match {
      case None => Left(RejectTarget(block.prop, block.obs, band, RejectTarget.Dec, lookup(dec, _.used), lookup(dec, _.limit)))
      case Some(updated) => Right(new DecResourceGroup(updated))
    }
  }

  private def addTooTime(bin: DecBin[BoundedTime], block: Block): DecBin[BoundedTime] =
    bin.updated(bin.binValue.reserve(block.time).get)

  // Try to distribute the time evenly across the dec bins.  If any of the bins
  // cannot handle their share, then we apply the time to the remaining bins.
  private def reserveToo(block: Block, band: QueueBand): RejectMessage Either DecResourceGroup =
    DecResourceGroup.tooBlocks(block, bins) match {
      case Some(s) => {
        val updatedBins = bins.bins.zip(s).map(
          binAndBlock => addTooTime(binAndBlock._1, binAndBlock._2)
        )
        Right(new DecResourceGroup(DecBinGroup.fromBins(updatedBins*)))
      }
      case _ =>
        val cur = bins.bins.foldLeft(Time.ZeroHours)(_ + _.binValue.used)
        val max = bins.bins.foldLeft(Time.ZeroHours)(_ + _.binValue.limit)
        Left(RejectTarget(block.prop, block.obs, band, RejectTarget.Dec, cur, max))
    }

  override def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either DecResourceGroup =
    block.prop.too match {
      case Too.none => reserveNormal(block, queue.band)
      case _ => reserveToo(block, queue.band)
    }

  /**
   * Reserves up-to the given amount of time associated with the declination
   * of the given target.  Returns a new DecResourceGroup and any time that
   * could not be reserved.
   */
  def reserveAvailable(time: Time, target: Target): (DecResourceGroup, Time) =
    reserveAvailable(time, target.dec)

  /**
   * Reserves up-to the given amount of time associated with the given
   * declination.  Returns a new DecResourceGroup and any time that
   * could not be reserved.
   */
  def reserveAvailable(time: Time, dec: Declination): (DecResourceGroup, Time) = {
    bins.get(dec) match {
      case Some(DecBin(_, bt)) =>
        val (newBoundedTime, remainingTime) = bt.reserveAvailable(time)
        val newBins = bins.updated(dec, newBoundedTime).get
        (new DecResourceGroup(newBins), remainingTime)
      case _ => (this, time)
    }
  }

}

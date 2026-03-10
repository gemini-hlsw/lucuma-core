// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.DecRanged
import edu.gemini.tac.qengine.api.config.DeclinationMap
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.block.TooBlocks
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage
import edu.gemini.tac.qengine.log.RejectTarget
import edu.gemini.tac.qengine.p1.ItacTarget
import edu.gemini.tac.qengine.util.BoundedTime
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ToOActivation
import lucuma.core.math.Declination
import lucuma.core.model.IntCentiPercent
import lucuma.core.util.TimeSpan

object DeclinationMapResource {
  def apply(t: TimeSpan, bins: DeclinationMap[IntCentiPercent]): DeclinationMapResource = {
    new DeclinationMapResource(bins.map(perc => BoundedTime(t *| perc)))
  }

  def tooBlocks(block: Block, grp: DeclinationMap[BoundedTime]): Option[Seq[Block]] =
    TooBlocks[DecRanged[BoundedTime]](block, grp.bins, _.binValue.remaining)
}

/**
 * A time reservation used to keep up with the time used at various dec
 * ranges.
 */
final case class DeclinationMapResource(val bins: DeclinationMap[BoundedTime]) extends Resource {
  type T = DeclinationMapResource

  private def lookup(dec: Declination, f: BoundedTime => TimeSpan): TimeSpan =
    bins.get(dec).map(bin => f(bin.binValue)).getOrElse(TimeSpan.Zero)

  def limit(dec: Declination): TimeSpan      = lookup(dec, _.limit)
  def limit(t: ItacTarget): TimeSpan       = limit(t.dec)

  def remaining(dec: Declination): TimeSpan  = lookup(dec, _.remaining)
  def remaining(t: ItacTarget): TimeSpan   = remaining(t.dec)

  def isFull(dec: Declination): Boolean  = remaining(dec).isZero
  def isFull(t: ItacTarget): Boolean   = isFull(t.dec)

  private def reserveNormal(block: Block, band: ScienceBand): RejectMessage Either DeclinationMapResource = {
    val dec = block.obs.itacTarget.dec
    bins.updated(dec, _.reserve(block.time)) match {
      case None => Left(RejectTarget(block.prop, block.obs, band, RejectTarget.Dec, lookup(dec, _.used), lookup(dec, _.limit)))
      case Some(updated) => Right(new DeclinationMapResource(updated))
    }
  }

  private def addTooTime(bin: DecRanged[BoundedTime], block: Block): DecRanged[BoundedTime] =
    bin.updated(bin.binValue.reserve(block.time).get)

  // Try to distribute the time evenly across the dec bins.  If any of the bins
  // cannot handle their share, then we apply the time to the remaining bins.
  private def reserveToo(block: Block, band: ScienceBand): RejectMessage Either DeclinationMapResource =
    DeclinationMapResource.tooBlocks(block, bins) match {
      case Some(s) => {
        val updatedBins = bins.bins.zip(s).map(
          binAndBlock => addTooTime(binAndBlock._1, binAndBlock._2)
        )
        Right(new DeclinationMapResource(DeclinationMap.fromBins(updatedBins*)))
      }
      case _ =>
        val cur = bins.bins.foldLeft(TimeSpan.Zero)(_ +| _.binValue.used)
        val max = bins.bins.foldLeft(TimeSpan.Zero)(_ +| _.binValue.limit)
        Left(RejectTarget(block.prop, block.obs, band, RejectTarget.Dec, cur, max))
    }

  override def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either DeclinationMapResource =
    block.prop.too match {
      case ToOActivation.None => reserveNormal(block, queue.band)
      case _ => reserveToo(block, queue.band)
    }

  /**
   * Reserves up-to the given amount of time associated with the declination
   * of the given target.  Returns a new DeclinationMapResource and any time that
   * could not be reserved.
   */
  def reserveAvailable(time: TimeSpan, target: ItacTarget): (DeclinationMapResource, TimeSpan) =
    reserveAvailable(time, target.dec)

  /**
   * Reserves up-to the given amount of time associated with the given
   * declination.  Returns a new DeclinationMapResource and any time that
   * could not be reserved.
   */
  def reserveAvailable(time: TimeSpan, dec: Declination): (DeclinationMapResource, TimeSpan) = {
    bins.get(dec) match {
      case Some(DecRanged(_, bt)) =>
        val (newBoundedTime, remainingTime) = bt.reserveAvailable(time)
        val newBins = bins.updated(dec, newBoundedTime).get
        (new DeclinationMapResource(newBins), remainingTime)
      case _ => (this, time)
    }
  }

}

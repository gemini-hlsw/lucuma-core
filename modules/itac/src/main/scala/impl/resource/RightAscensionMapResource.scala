// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.RightAscensionMap
import edu.gemini.tac.qengine.api.config.SiteSemesterConfig
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.impl.block.TooBlocks
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.log.RejectMessage
import edu.gemini.tac.qengine.log.RejectToo
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ToOActivation
import lucuma.core.model.ConstraintSet

object RightAscensionMapResource {
  // Creates an RA resource group from the site/semester configuration.
  def apply(c: SiteSemesterConfig): RightAscensionMapResource =
    new RightAscensionMapResource(c.raLimits.map(PerRightAscensionResource(_, c)))
}

/**
 * TODO: Rename "SpatialBinResourceGroup"?
 *
 * A resource that encapsulates RightAscensionMap[PerRightAscensionResource] (n.b. PerRightAscensionResource contains a DeclinationMapResource encapsulating a DeclinationMap)
 */
case class RightAscensionMapResource(val grp: RightAscensionMap[PerRightAscensionResource]) extends Resource {
  type T = RightAscensionMapResource

  def reserve(block: Block, queue: ProposalQueueBuilder): RejectMessage Either RightAscensionMapResource =
    if (block.prop.too != ToOActivation.None) reserveToo(block, queue) else reserveNonToo(block, queue)

  // Splits the block into one block/RaReservation according to the amount of
  // time to distribute to each RaReservation.  If the split is successful,
  // then we can record time in each of the RaReservations.  Otherwise, the
  // Too observation cannot be scheduled.
  private def reserveToo(block: Block, queue: ProposalQueueBuilder): RejectMessage Either RightAscensionMapResource =
    tooBlocks(block) match {
        case None => {
          val sum = grp.bins.foldLeft(Time.hours(0))(_ + _.remaining(block.obs.conditions))
          Left(new RejectToo(block.prop, block.obs, queue.band, sum))
        }
        case Some(s) =>
          Right(
            new RightAscensionMapResource(
              RightAscensionMap(
                grp.bins.zip(s) map {
                  case (raResr, blk) => raResr.reserve(blk, queue).toOption.get
                }
              )
            )
          )
    }

  private def reserveNonToo(block: Block, queue: ProposalQueueBuilder): RejectMessage Either RightAscensionMapResource = {
    val ra = block.obs.target.ra
    grp(ra).reserve(block, queue).map(bin => new RightAscensionMapResource(grp.updated(ra, bin)))
  }

  def tooBlocks(block: Block): Option[Seq[Block]] =
    TooBlocks[PerRightAscensionResource](block, grp.bins, _.remaining(block.obs.conditions))

  /**
   * Reserves up-to the given amount of time, returning an updated
   * RightAscensionMapResource and any time left over that could not be reserved.
   */
  def reserveAvailable(time: Time, target: ItacTarget, conds: ConstraintSet): (RightAscensionMapResource, Time) = {
    val (bin, rem) = grp(target.ra).reserveAvailable(time, target, conds)
    (new RightAscensionMapResource(grp.updated(target.ra, bin)), rem)
  }

  def reserveAvailable(reduction: Observation): (RightAscensionMapResource, Time) =
    reserveAvailable(reduction.time, reduction.target, reduction.conditions)

  def reserveAvailable(reductions: List[Observation]): (RightAscensionMapResource, Time) = {
    reductions.foldLeft((this,Time.Zero)) {
      case ((grp0, time), reduction) =>
        grp0.reserveAvailable(reduction) match {
          case (newGrp, leftover) => (newGrp, leftover+time)
        }
    }
  }

}
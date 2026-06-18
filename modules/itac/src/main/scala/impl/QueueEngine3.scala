// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import cats.data.State
import cats.syntax.all.*
// import edu.gemini.tac.qengine.api.BucketsAllocation
// import edu.gemini.tac.qengine.api.QueueCalc
// import edu.gemini.tac.qengine.api.config.ConditionsCategory
import edu.gemini.tac.qengine.api.config.QueueEngineConfig
import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.api.queue.ProposalQueue
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.impl.block.BlockIterator
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
// import edu.gemini.tac.qengine.impl.resource.PerRightAscensionResource
import edu.gemini.tac.qengine.impl.resource.RightAscensionMapResource
import edu.gemini.tac.qengine.impl.resource.SemesterResource
// import edu.gemini.tac.qengine.log.AcceptMessage
import edu.gemini.tac.qengine.log.ProposalLog
// import edu.gemini.tac.qengine.log.RemovedRejectMessage
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.BoundedTime
// import lucuma.core.data.Metadata
import lucuma.core.enums.ScienceBand
// import lucuma.core.enums.ScienceBand.*
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.Enumerated
// import lucuma.core.model.IntCentiPercent
// import edu.gemini.tac.qengine.api.config.Default

object QueueEngine3 { //extends QueueEngine {

  def calc(
    proposals:    List[Proposal],
    queueTimes:   (ScienceBand, Site) => QueueTime,
    config:       QueueEngineConfig,
  ): (SemesterResource, ProposalLog, List[ProposalQueue]) = {

    // Find all the observations that don't participate in the queue process, because their time
    // needs to be subtracted from the initail RightAscensionMapResource (which happens on construction). Then
    // finish building our SemesterResource
    val rolloverObs: List[ItacObservation]       = Nil 
    val classicalProps    = proposals.filter(_.tpe.scienceSubtype == ScienceSubtype.Classical)
    val classicalObs      = classicalProps.flatMap(_.obsList)
    val rightAscensionMapResource   = RightAscensionMapResource(config.binConfig).reserveAvailable(rolloverObs ++ classicalObs)._1
    val compositeTimeRestrictionResource: List[TimeRestriction[BoundedTime]] = Nil // do we need any of these?
    val semesterResource  = new SemesterResource(rightAscensionMapResource, compositeTimeRestrictionResource)

    // We're done with classical proposals. Filter them out.
    val queueProposals: List[Proposal] =
      proposals.filter(_.tpe.scienceSubtype != ScienceSubtype.Classical)

    // BlockIterator for a given site and band.
    def iteratorFor(band: ScienceBand, site: Site): BlockIterator =
      BlockIterator(
        queueTimes(band, site).TimeAccountingCategoryQuanta,
        config.timeAccountingCategorySeq.sequence,
        TimeAccountingCategory
          .values
          .toList
          .fproduct: cat =>
            queueProposals.map(_.shardFor(site, cat, band))
          .toMap,
        p => p.observations
      )

    // Build a queue for each site+band combination, in ascending order by band, alternating between sites.
    val ((remaining, log), queues) =
      (Enumerated[ScienceBand].all, Enumerated[Site].all)
        .tupled
        .traverse: (band, site) => 
          State[(SemesterResource, ProposalLog), ProposalQueue]: (res, log) =>
            val stage = QueueCalcStage.compute(
              queue       = ProposalQueueBuilder(queueTimes(band, site), band, site),
              iter        = iteratorFor(band, site), 
              activeList  = _.observations,
              res         = res,
              log         = log,
            )
            ((stage.resource, stage.log), stage.queue)
        .run((semesterResource, ProposalLog.Empty))
        .value
 
    // ACTUALLY we should do the following after each pass in order to free up time. There's no
    // point allocating time in the band 2 shard if there's not time for the band 1 shard.

    // We now have queues for every (site, band) and can show results similar to what we did in OCS
    // for each. But for the final queue (there is only one) we need to discard any proposals with
    // kicked-out shards and add the shards' times back to the partners. This is an error condition and
    // it indicates times need to be adjusted. 


    (remaining, log, queues)


    // val ((finalResource, band123log), (queue1WithoutClassical, queue2, queue3)) = (
    //   runQueue(Band1), runQueue(Band2), runQueue(Band3)
    // ).tupled.run((semesterResource, ProposalLog.Empty)).value

    // // Add classical proposals back to Band 1
    // val queue1 = new ProposalQueue {
    //   def band      = queue1WithoutClassical.band
    //   def queueTime = queue1WithoutClassical.queueTime
    //   def toList    = queue1WithoutClassical.toList ++ classicalProps
    // }

    // // All Band 4 proposals that made it to ITAC are accepted.
    // val queue4 = new ProposalQueue {
    //   def band      = Band4
    //   def queueTime = queueTimes(Band4)
    //   def toList    = queueProposals(Band4)
    // }

    // // Band 4 proposals need to go into the log.
    // val band1234log: ProposalLog =
    //   queue4.toList.foldLeft(band123log)((l, p) => l.updated(p.id, Band4, AcceptMessage(p)))

    // Removed proposals need to go into the log.
    // val finalLog: ProposalLog =
    //   removed.foldLeft(band1234log)((l, p) => l.updated(p.id, Band1, RemovedRejectMessage(p)))

    // // Assemble our final result for the user
    // new QueueCalc {
    //   export config.binConfig.{ site, semester }
    //   val proposalLog       = finalLog
    //   val bucketsAllocation = BucketsAllocationImpl(finalResource.ra.grp.bins.toList)
    //   def queue(b: ScienceBand) =
    //     b match {
    //       case Band1 => queue1
    //       case Band2 => queue2
    //       case Band3 => queue3
    //       case Band4 => queue4
    //     }
    // }

  }

}


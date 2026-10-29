// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import cats.data.State
import cats.syntax.all.*
import edu.gemini.tac.qengine.api.QueueCalc
import edu.gemini.tac.qengine.api.config.QueueEngineConfig
import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.api.queue.ProposalQueue
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.impl.block.BlockIterator
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.impl.resource.RightAscensionMapResource
import edu.gemini.tac.qengine.impl.resource.SemesterResource
import edu.gemini.tac.qengine.log.ProposalLog
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.BoundedTime
import lucuma.core.data.PerSite
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.Enumerated
import edu.gemini.tac.qengine.api.BucketsAllocation
import cats.data.NonEmptyList
import lucuma.core.model.ProposalReference
import edu.gemini.tac.qengine.log.RejectMessage
import edu.gemini.tac.qengine.log.ProposalLog.Entry

object QueueEngine3:

  def calc(proposals:    List[Proposal],
    queueTimes:   (ScienceBand, Site) => QueueTime,
    config:       PerSite[QueueEngineConfig], // but this is only for one site!
  ): QueueCalc =
    apply(proposals, queueTimes, config).calc

case class QueueEngine3(
  proposals:    List[Proposal],
  queueTimes:   (ScienceBand, Site) => QueueTime,
  config:       PerSite[QueueEngineConfig], // but this is only for one site!
) {

  // Find all the observations that don't participate in the queue process, because their time
  // needs to be subtracted from the initail RightAscensionMapResource (which happens on construction). Then
  // finish building our SemesterResource
  // val rolloverObs: List[ItacObservation]       = Nil 
  // val classicalProps    = proposals.filter(_.tpe.scienceSubtype == ScienceSubtype.Classical)
  // val classicalObs      = classicalProps.flatMap(_.obsList)
  val rightAscensionMapResource   = config.map(c => RightAscensionMapResource(c.binConfig)) //.reserveAvailable(rolloverObs ++ classicalObs)._1
  val compositeTimeRestrictionResource: List[TimeRestriction[BoundedTime]] = Nil // do we need any of these?
  val semesterResource  = rightAscensionMapResource.map(SemesterResource(_, compositeTimeRestrictionResource))

  // We're done with classical proposals. Filter them out.
  val queueProposals: List[Proposal] =
    proposals
      .filter(_.tpe.scienceSubtype != ScienceSubtype.Classical)
      .sortBy(_.ranking.value)

  // BlockIterator for a given site and band.
  def iteratorFor(candidates: List[Proposal], band: ScienceBand, site: Site): BlockIterator =
    BlockIterator(
      queueTimes(band, site).TimeAccountingCategoryQuanta,
      config(site).timeAccountingCategorySeq.sequence,
      Enumerated[TimeAccountingCategory]
        .all
        .fproduct: cat =>
          candidates.map(_.shardFor(site, cat, band)).filterNot(_.isEmpty)
        .toMap,
      p => p.observations
    )

  def tryCalc(candidates: List[Proposal], initialLog: ProposalLog) =
    (Enumerated[ScienceBand].all, Enumerated[Site].all)
      .tupled
      .traverse: (band, site) => 
        State[(PerSite[SemesterResource], ProposalLog), ProposalQueue]: (res, log) =>
          val stage = QueueCalcStage.compute(
            queue       = ProposalQueueBuilder(queueTimes(band, site), band, site),
            iter        = iteratorFor(candidates, band, site), 
            activeList  = _.observations,
            res         = res(site),
            log         = log,
          )
          ((res.put(site, stage.resource), stage.log), stage.queue)
      .run((semesterResource, initialLog))
      .value

  def calc: QueueCalc = {

    def go(candidates: List[Proposal], currentLog: ProposalLog): QueueCalc = {

      // println("---- calc.go: currentLog =")
      // currentLog.toList.foreach: 
      //   case ProposalLog.Entry(key, m: RejectMessage) =>
      //     println(s"  ${key.id} - ${m.prop.parentProposal.piName} - ${m.detail}")
      //   case e => ()
      // println()

      val ((remaining, log), queues) = tryCalc(candidates, currentLog)

      // println("---- calc.go: next log =")
      // log.toList.foreach: 
      //   case ProposalLog.Entry(key, m: RejectMessage) =>
      //     println(s"  ${key.id} - ${m.prop.parentProposal.piName} - ${m.detail}")
      //   case e => ()
      // println()

      // Look at the reject list. If there are any rejected shards that have associated *accepted* shards 
      // then we need to remove the accepted ones and re-run the queue.

      val allShards: List[ProposalShard] = candidates.flatMap(_.shards).filterNot(_.isEmpty)
      val allAcceptedShards: List[ProposalShard] = queues.flatMap(_.toList)
      val allRejectedShards: List[ProposalShard] = allShards.filterNot(s => allAcceptedShards.exists(_.reference === s.reference))

      allRejectedShards.foreach: s =>
        println(s"rejected or not considered: ${s.parentProposal.piName} - ${s.reference}")

      val mustAlsoReject: Option[NonEmptyList[ProposalShard]] =
        NonEmptyList.fromList:
          allRejectedShards.flatMap: rs =>
            allAcceptedShards.filter(_.parentProposal.reference === rs.parentProposal.reference)

      mustAlsoReject match
        case None =>
          QueueCalc(
            PerSite.unfold: site =>
              band => 
                queues
                  .find(q => q.site === site && q.band === band)
                  .getOrElse(sys.error(s"unpossible: missing queue for $site $band")),
            log,
            remaining.map(rsrc => BucketsAllocation(rsrc.ra.grp.bins.toList))
          )
        case Some(nel) =>

          // Kick out the lowest-ranked proposale that has a rejected shard, then try again
          val toKickOut: Proposal = 
            nel.sortBy(_.parentProposal.ranking.value).last.parentProposal

          val shardsToKickOut: List[ProposalShard] =
            nel.filter(_.parentProposal.reference === toKickOut.reference)

          val previousPartialRejects: Set[ProposalReference] =
            log
              .toList
              .collect: 
                case ProposalLog.Entry(_, RejectPartial(prop)) => prop.parentProposal.reference
              .toSet
              
          val allPartialRejects = previousPartialRejects + toKickOut.reference

          val relevantLogEntriesToPreserve = 
            ProposalLog:
              log
                .toList
                .filter:
                  // case ProposalLog.Entry(_, AcceptMessage(_)) => false
                  case ProposalLog.Entry(k, _) if allPartialRejects(k.id.parentReference) => true       
                  case _ => false                       

          val newLog =
            shardsToKickOut.foldLeft(relevantLogEntriesToPreserve): (log, shard) =>
              log.updated(shard.reference, shard.allocation.scienceBand, RejectPartial(shard))

          println(s"${toKickOut.reference.label} (${toKickOut.piName}) was partially accepted and partially rejected. Re-running without it...")
          go(candidates.filterNot(_.reference === toKickOut.reference), newLog)

    }



    go(queueProposals, ProposalLog.Empty)


    // ACTUALLY we should do the following after each pass in order to free up time. There's no
    // point allocating time in the band 2 shard if there's not time for the band 1 shard.

    // We now have queues for every (site, band) and can show results similar to what we did in OCS
    // for each. But for the final queue (there is only one) we need to discard any proposals with
    // kicked-out shards and add the shards' times back to the partners. This is an error condition and
    // it indicates times need to be adjusted. 





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

case class RejectPartial(prop: ProposalShard) extends RejectMessage:
  val detail: String = "Another shard in this proposal was rejected."
  val reason: String = reason

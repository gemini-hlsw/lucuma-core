// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package edu.gemini.tac.qengine.impl

// import cats.data.State
// import cats.syntax.all.*
// import edu.gemini.tac.qengine.api.BucketsAllocation
// import edu.gemini.tac.qengine.api.QueueCalc
// import edu.gemini.tac.qengine.api.QueueEngine
// import edu.gemini.tac.qengine.api.config.ConditionsCategory
// import edu.gemini.tac.qengine.api.config.QueueEngineConfig
// import edu.gemini.tac.qengine.api.config.TimeRestriction
// import edu.gemini.tac.qengine.api.queue.ProposalQueue
// import edu.gemini.tac.qengine.api.queue.time.QueueTime
// import edu.gemini.tac.qengine.impl.block.BlockIterator
// import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
// import edu.gemini.tac.qengine.impl.resource.PerRightAscensionResource
// import edu.gemini.tac.qengine.impl.resource.RightAscensionMapResource
// import edu.gemini.tac.qengine.impl.resource.SemesterResource
// import edu.gemini.tac.qengine.log.AcceptMessage
// import edu.gemini.tac.qengine.log.ProposalLog
// import edu.gemini.tac.qengine.log.RemovedRejectMessage
// import edu.gemini.tac.qengine.p1.*
// import edu.gemini.tac.qengine.util.BoundedTime
// import lucuma.core.enums.ScienceBand
// import lucuma.core.enums.ScienceBand.*
// import lucuma.core.enums.ScienceSubtype
// import lucuma.core.enums.TimeAccountingCategory
// import lucuma.core.util.Enumerated
// import lucuma.core.enums.Site

// object QueueEngine3 { //extends QueueEngine {

//   def calc(
//     proposals:    List[ItacProposal],
//     queueTimes:   (ScienceBand, Site) => QueueTime,
//     config:       QueueEngineConfig,
//   ): QueueCalc = {

//     // Find all the observations that don't participate in the queue process, because their time
//     // needs to be subtracted from the initail RightAscensionMapResource (which happens on construction). Then
//     // finish building our SemesterResource
//     // val rolloverObs: List[ItacObservation]       = ???
//     // val classicalProps    = siteProposals(Band1).filter(_.mode == ScienceSubtype.Classical)
//     // val classicalObs      = classicalProps.flatMap(_.obsList)
//     // val rightAscensionMapResource   = RightAscensionMapResource(config.binConfig).reserveAvailable(rolloverObs ++ classicalObs)._1
//     // val compositeTimeRestrictionResource: List[TimeRestriction[BoundedTime]] = Nil
//     // val semesterResource  = new SemesterResource(rightAscensionMapResource, compositeTimeRestrictionResource)

//     // We're done with classical proposals. Filter them out.
//     // val queueProposals: ScienceBand => List[Proposal] =
//     //   siteProposals.map(_.filter(_.mode != ScienceSubtype.Classical))

//     // BlockIterator for a given site (some observations are filtered out) and band (some proposals are filtered out).
//     def iteratorFor(band: ScienceBand, site: Site): BlockIterator =
//       BlockIterator(
//         queueTimes(band, site).TimeAccountingCategoryQuanta,
//         config.TimeAccountingCategorySeq.sequence,
//         TimeAccountingCategory
//           .values
//           .toList
//           .fproduct: tac =>
//             proposals.filter(_.allocations.exists(_.category === tac))
//           .toMap,
//         p => p.itacObservationsScaledForSiteAndBand(site, band) 
//       )

//     // Build a queue for each site+band combination, in ascending order by band, alternating between sites.
//     // The resulting queues will be combined somehow.
//     val z = (Enumerated[ScienceBand].all, Enumerated[Site].all)
//       .tupled
//       .traverse: (band, site) => 
//         State[(SemesterResource, ProposalLog), ProposalQueue]: (res, log) =>
//           val stage = QueueCalcStage.newInstance(
//             queue       = ProposalQueueBuilder.Empty,
//             iter        = iteratorFor(band, site), 
//             activeList  = _.itacObservationsScaledForSiteAndBand(site, band),
//             res         = res,
//             log         = log,
//           )
//           ((stage.resource, stage.log), stage.queue)
//       .run((???, ProposalLog.Empty))

//     // val ((finalResource, band123log), (queue1WithoutClassical, queue2, queue3)) = (
//     //   runQueue(Band1), runQueue(Band2), runQueue(Band3)
//     // ).tupled.run((semesterResource, ProposalLog.Empty)).value

//     // // Add classical proposals back to Band 1
//     // val queue1 = new ProposalQueue {
//     //   def band      = queue1WithoutClassical.band
//     //   def queueTime = queue1WithoutClassical.queueTime
//     //   def toList    = queue1WithoutClassical.toList ++ classicalProps
//     // }

//     // // All Band 4 proposals that made it to ITAC are accepted.
//     // val queue4 = new ProposalQueue {
//     //   def band      = Band4
//     //   def queueTime = queueTimes(Band4)
//     //   def toList    = queueProposals(Band4)
//     // }

//     // // Band 4 proposals need to go into the log.
//     // val band1234log: ProposalLog =
//     //   queue4.toList.foldLeft(band123log)((l, p) => l.updated(p.id, Band4, AcceptMessage(p)))

//     // Removed proposals need to go into the log.
//     // val finalLog: ProposalLog =
//     //   removed.foldLeft(band1234log)((l, p) => l.updated(p.id, Band1, RemovedRejectMessage(p)))

//     // // Assemble our final result for the user
//     // new QueueCalc {
//     //   export config.binConfig.{ site, semester }
//     //   val proposalLog       = finalLog
//     //   val bucketsAllocation = BucketsAllocationImpl(finalResource.ra.grp.bins.toList)
//     //   def queue(b: ScienceBand) =
//     //     b match {
//     //       case Band1 => queue1
//     //       case Band2 => queue2
//     //       case Band3 => queue3
//     //       case Band4 => queue4
//     //     }
//     // }

//     ???

//   }

// }





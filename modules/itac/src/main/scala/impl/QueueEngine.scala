// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// import queue.FinalProposalQueue
// import resource._

// import collection.immutable.List
// import edu.gemini.tac.qengine.p1._
// import edu.gemini.tac.qengine.api.{BucketsAllocation, QueueCalc}
// import edu.gemini.tac.qengine.log.ProposalLog
// import edu.gemini.tac.qengine.api.queue.ProposalQueue
// import edu.gemini.tac.qengine.ctx.Context
// import edu.gemini.spModel.core.Site
// import edu.gemini.tac.qengine.api.config.{ConditionsCategory, QueueEngineConfig, SiteSemesterConfig}
// import edu.gemini.tac.qengine.p2.rollover.RolloverObservation

// import collection.immutable.List._
// import edu.gemini.tac.qengine.api.queue.time.QueueTime

// import edu.gemini.tac.qengine.util.BoundedTime
// import org.slf4j.LoggerFactory
// import edu.gemini.tac.qengine.util.Time
// import scalaz._, Scalaz._
// import edu.gemini.tac.qengine.log.AcceptMessage
// import edu.gemini.tac.qengine.log.RejectMessage

// object QueueEngine extends edu.gemini.tac.qengine.api.QueueEngine {
//   val Log = LoggerFactory.getLogger("edu.gemini.itac")

//   case class RaAllocation(name: String, boundedTime: BoundedTime)
//   case class BucketsAllocationImpl(raBins: List[PerRightAscensionResource]) extends BucketsAllocation {

//     sealed trait Row extends Product with Serializable
//     case class RaRow(h: String, remaining: Double, used: Double, limit: Double) extends Row
//     case class ConditionsRow(t: ConditionsCategory, u: Double, r: Double, l: Double) extends Row

//     val hPerBin  = 24 / raBins.length
//     val binHours = 0 until 24 by 24 / raBins.length
//     val raRanges = binHours.map(h => s"$h-${h + hPerBin} h")
//     val report = raRanges.zip(raBins).toList.map {
//       case (h, b) =>

//         val binUsedMins: Double =
//           b.condsRes.bins.bins.values.map(_.used.toMinutes.value).sum

//         val ra = RaRow(
//           h,
//           math.round(b.remaining.toMinutes.value) / 60.0,
//           math.round(binUsedMins) / 60.0,
//           math.round(b.limit.toMinutes.value) / 60.0
//         )

//         val conds = b.condsRes.bins.bins.toList.sortBy(_._1.name).map {
//           case (c, t) =>
//             ConditionsRow(
//               c,
//               math.round(t.used.toMinutes.value) / 60.0,
//               (math.round(t.remaining.toMinutes.value) / 60.0) min ra.remaining,
//               math.round(t.limit.toMinutes.value) / 60.0
//             )
//         }
//         //s"$ra\n${conds.mkString("\n")}"
//         ra :: conds
//     }

//     override def toString = {
//       report.mkString("\n")
//       //BucketsAllocationImpl(Nil)
//       //System.exit(0)
//     }

//     // Annoying, we need to turn off ANSI color if output is being redirected. In the `main` project
//     // we have a `Colors` module for this but in `engine` there's no such thing we we'll just hack
//     // it in again.
//     def embolden(s: String): String =
//       if (System.console != null || sys.props.get("force-color").isDefined) s"${Console.BOLD}$s${Console.RESET}"
//       else s

//     val raTablesANSI: String =
//       report.flatten.map {
//         case RaRow(h, r, u, l)         => embolden(f"\nRA: $h%-78s  $l%6.2f  $u%6.2f  $r%6.2f")
//         case ConditionsRow(t, u, r, l) => f"Conditions: $t%-70s  $l%6.2f  $u%6.2f  $r%6.2f "
//       } .mkString("\n")

//   }


//   private final class QueueCalcImpl(val context: Context, val queue: ProposalQueue, val proposalLog: ProposalLog, val bucketsAllocation: BucketsAllocation) extends QueueCalc

//   private[impl] def classicalProps(allProps: List[Proposal], site: Site): List[Proposal] =
//     allProps filter { p => (p.mode == Mode.Classical) && (p.site == site) }

//   private[impl] def classicalObs(allProps: List[Proposal], site: Site): List[Observation] =
//     classicalProps(allProps, site) flatMap { p => p.relativeObsList(ScienceBand.QBand1) }

//   private[impl] def initBins(config: SiteSemesterConfig, rollover: List[RolloverObservation], props: List[Proposal]): RightAscensionMapResource = {
//     val cattimes = rollover ++ classicalObs(props, config.site)
//     RightAscensionMapResource(config).reserveAvailable(cattimes)._1
//   }

//   private[impl] def initBins(config: QueueEngineConfig, props: List[Proposal]): RightAscensionMapResource =
//     initBins(config.binConfig, config.rollover.obsList, props)

//   def filterProposals(proposals: List[Proposal], config: QueueEngineConfig): List[Proposal] = {
//     proposals.filter(p => p.site == config.binConfig.site && p.mode.schedule) // remove non-site and non-queue
//   }

//   /**
//    * Filters out proposals from the other site. Initializes bins using that list. *Then* removes non-queue proposals.
//    * Note that this means that the RightAscensionMapResource returned is initialized from non-queue and queue proposals
//    */
//   def filterProposalsAndInitializeBins(proposals: List[Proposal], config: QueueEngineConfig): (List[Proposal], RightAscensionMapResource) = {
//     val siteProps = filterProposals(proposals, config) // filter out non-site and non-queue (classical time has already been subtracted)
//     val bins = initBins(config, siteProps)
//     (siteProps, bins)
//   }

//   // /** Log the time availability and usage for a stage. */
//   // def show(config: QueueEngineConfig, s: QueueCalcStage): Unit = {
//   //   Log.info(s"${Console.GREEN}-----------------------------------${Console.RESET}")
//   //   Log.info(s"${Console.GREEN}TimeAccountingCategory       Band 1/2       Band 3${Console.RESET}")
//   //   config.TimeAccountingCategorys.foreach { p =>
//   //     val q   = s.queue
//   //     // val t0  = q.queueTime(p).toHours.value
//   //     val b12Aval = q.queueTime(ScienceBand.Category.B1_2, p).toHours.value
//   //     val b12Used = q.usedTime(ScienceBand.Category.B1_2, p).toHours.value
//   //     val b3Aval = q.queueTime(ScienceBand.Category.B3, p).toHours.value
//   //     val b3Used = q.usedTime(ScienceBand.Category.B3, p).toHours.value
//   //     Log.info(f"${p.id}%-10s $b12Used%5.1f/$b12Aval%5.1f  $b3Used%5.1f/$b3Aval%5.1f")
//   //   }
//   //   Log.info(s"${Console.GREEN}-----------------------------------${Console.RESET}")
//   // }

//   def calc(bandedProposals: Map[ScienceBand, List[Proposal]], queueTime: QueueTime, config: QueueEngineConfig, extras: List[Proposal], removed: List[Proposal]): QueueCalc = {

//     // temp
//     val proposals = bandedProposals.values.toList.flatten

//     // (helper) run and log a queue calculation stage, using our local config.
//     def stage(params: QueueCalcStage.Params): QueueCalcStage = {
//       val stage = QueueCalcStage(params)
//       // show(config, stage)
//       // stage.queue.bandedQueue.foreach { case (b, ps) =>
//       //     println(s"$b")
//       //     ps.foreach(p => println(s"  ${p.id.reference}"))
//       // }
//       stage
//     }

//     // Identify the valid proposals and discard the others.
//     val (validProposals, bins) = filterProposalsAndInitializeBins(proposals, config)
//     val initialCandidates = ProposalPrep(validProposals)

//     // Run a queue calc stage for bands 1 and 2. This is the same way it used to work, but we're
//     // only doing it once because we're not re-using the iterator.
//     val stageWithBands12 = {
//       val candidates = initialCandidates
//       val params = QueueCalcStage.Params.band12(
//         grouped = candidates.group,
//         log     = candidates.log,
//         qtime   = queueTime,
//         config  = config,
//         bins    = bins
//       )
//       stage(params)
//     }

//     // Now add the band 3 programs. We do this by pretending the used time in bands 1/2 is all the
//     // time we had in those bands, which is kind of an ugly hack but it works. Everything goes into
//     // band 3, up to TimeAccountingCategory time limits.
//     val b3candidates = initialCandidates.remove(stageWithBands12.queue.toList).band3(stageWithBands12.log)
//     val stageWithBands123 = {
//       val params = QueueCalcStage.Params.band3(
//         config       = config,
//         grouped      = b3candidates.group,
//         phase12bins  = stageWithBands12.resource,
//         phase12log   = b3candidates.log,
//         phase12queue = stageWithBands12.queue,
//       )
//       val s = stage(params)
//       s
//     }

//     // Construct our final queue, which requires a bit of rejiggering.
//     val (finalQueue, finalLog) = {

//       // The band 1/2 boundary is wrong using the old strategy so we'll regroup them. First collect
//       // the band 1/2 programs IN ORDER.
//       val band12proposals: List[Proposal] =
//         stageWithBands123.queue.bandedQueue.get(ScienceBand.QBand1).orZero ++
//         stageWithBands123.queue.bandedQueue.get(ScienceBand.QBand2).orZero

//       // Now re-group bands 1 and 2 using TimeAccountingCategory-specific time buckets
//       val band12map: Map[ScienceBand, List[Proposal]] =
//         config.TimeAccountingCategorys.foldMap { pa =>

//           // Within a given TimeAccountingCategory we can map used time to band.
//           def band(t: Time): ScienceBand = {
//             val b1 = queueTime(ScienceBand.QBand1, pa) // exactly. we never overfill band 1

//             // NOTE: the following doesn't work because the engine sometimes overfills slightly past
//             // the limit. In these cases we *can't* push the last proposal to B3 because it's using
//             // the wrong set of observations. So just leave it in B2 in that case.
//             // val b2 = queueTime(ScienceBand.QBand2, pa).percent(105)
//             // if (t <= b1) ScienceBand.QBand1 else if (t <= (b1 + b2)) ScienceBand.QBand2 else ScienceBand.QBand3
//             if (t <= b1) ScienceBand.QBand1 else ScienceBand.QBand2
//           }

//           // Go through all the proposals for this TimeAccountingCategory adding each to its band, based on the
//           // accumulated used time. This is why they need to be in order above.
//           band12proposals
//             .filter(_.ntac.TimeAccountingCategory == pa)
//             .foldLeft((Time.Zero, Map.empty[ScienceBand, List[Proposal]])) { case ((t, m), p) =>
//               val tʹ = t + p.time
//               (tʹ, m |+| Map(band(tʹ) -> List(p)))
//           } ._2

//         }

//       // Band4 is a simple calculation
//       val band4 = PoorWeatherCalc(initialCandidates.remove(stageWithBands123.queue.toList).propList)

//       // Ok so *replace* bands 1/2 and then add band 4.
//       val bandedQueue1  = stageWithBands123.queue.bandedQueue ++ band12map
//       val bandedQueue2  = bandedQueue1 + (ScienceBand.QBand4 -> band4)

//       // Some proposals end up in the wrong bands and we want to override this. So those that are
//       // mentioned in explicitQueueAssignments will get moved here.
//       val bandedQueue3 = config.explicitQueueAssignments.toList.foldRight(bandedQueue2) { case ((ref, band), bq) =>
//         // remove proposal from current queue, wherever it is, if it's there at all
//         bq.map { case (k, v) => k -> v.filterNot(_.ntac.reference == ref) } |+|
//         // and put it in the explicit band (if we can find it)
//         Map(band -> proposals.filter(_.ntac.reference == ref))
//       }

//       // Finally, add extras to the queue. These are proposals that don't go through the queue
//       // process but we know where they're supposed to end up. We also need to add them to the log!
//       val (bandedQueue4, finalLog) = {
//         filterProposals(extras, config).foldRight((bandedQueue3, stageWithBands123.log)) { case (p, (q, log)) =>
//           config.explicitQueueAssignments.get(p.ntac.reference) match {
//             case None => sys.error(s"No explicitQueueAssignments element was specified for ${p.ntac.reference}, check your Gx-queue.yaml file.")
//             case Some(b) => (q |+| Map(b -> List(p)), log.updated(p.id, b.logCategory, AcceptMessage(p, BoundedTime(Time.Zero), BoundedTime(Time.Zero))))
//           }
//         }
//       }

//       // Add `removed` propodals to the log so they show up as rejects instead of as orphans. This
//       // makes it clear we didn't forget about them.
//       val finalLog2 = removed.foldLeft(finalLog) { (log, p) =>
//         log.updated(p.id, ScienceBand.Category.B1_2, RemovedRejectMessage(p))
//       }

//       // Done!
//       (new FinalProposalQueue(queueTime, bandedQueue4), finalLog2)

//     }

//     // And we're done.
//     new QueueCalcImpl(
//       context           = config.binConfig.context,
//       queue             = finalQueue,
//       proposalLog       = finalLog,
//       bucketsAllocation = BucketsAllocationImpl(stageWithBands123.resource.ra.grp.bins.toList)
//     )

//   }

//   case class RemovedRejectMessage(prop: Proposal) extends RejectMessage {
//     def reason: String = "Unknown."
//     def detail: String = "Proposal was removed from consideration."
//   }

// }

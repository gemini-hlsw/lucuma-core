// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import edu.gemini.tac.qengine.api.{ BucketsAllocation, QueueCalc, QueueEngine }
import edu.gemini.tac.qengine.api.config.{ ConditionsCategory, QueueEngineConfig }
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.impl.block.BlockIterator
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.impl.resource.{ RaResource, RaResourceGroup, SemesterResource, TimeResourceGroup }
import edu.gemini.tac.qengine.log.{ ProposalLog, RemovedRejectMessage, AcceptMessage }
import edu.gemini.tac.qengine.p1._
import edu.gemini.tac.qengine.p1.QueueBand._
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.api.queue.ProposalQueue
import scalaz._, Scalaz._

object QueueEngine2 extends QueueEngine {

  def calc(
    rawProposals: QueueBand => List[Proposal],
    queueTimes:   QueueBand => QueueTime,
    config:       QueueEngineConfig,
    removed:      List[Proposal]
  ): QueueCalc = {

    // Silently filter out proposals that are not at our site.
    val siteProposals: QueueBand => List[Proposal] =
      rawProposals.map(_.filter(_.site == config.site))

    // Ensure that everything is in a compatible band. For now we'll just throw if there's an issue.
    QueueBand.values.fproduct(siteProposals).foreach { case (b, ps) => ps.foreach { p => QueueEngineBandProblems.unsafeCheckAll(p, b) }}

    // Find all the observations that don't participate in the queue process, because their time
    // needs to be subtracted from the initail RaResourceGroup (which happens on construction). Then
    // finish building our SemesterResource
    val rolloverObs       = config.rollover.obsList
    val classicalProps    = siteProposals(QBand1).filter(_.mode == Mode.Classical)
    val classicalObs      = classicalProps.flatMap(_.obsList)
    val raResourceGroup   = RaResourceGroup(config.binConfig).reserveAvailable(rolloverObs ++ classicalObs)._1
    val timeResourceGroup = new TimeResourceGroup(Nil) // let's not do this for now
    val semesterResource  = new SemesterResource(raResourceGroup, timeResourceGroup, QBand1)

    // We're done with classical proposals. Filter them out.
    val queueProposals: QueueBand => List[Proposal] =
      siteProposals.map(_.filter(_.mode != Mode.Classical))

    // All we need to construct an obs accessor is the banc.
    def obsAccessor(band: QueueBand): Proposal => List[Observation] = { p =>
      if (band == QBand3) p.band3Observations else p.obsList
    }

    // It's a moutful!
    def proposalsGoupedByPartnerAndSortedByRanking(band: QueueBand): Map[Partner, List[Proposal]] =
      queueProposals(band).groupBy(_.ntac.partner).map { case (k, v) => (k, v.sortBy(_.ntac.ranking)) }

    // All we need to construct a BlockIterator is the band.
    def iteratorFor(band: QueueBand): BlockIterator =
      BlockIterator(
        queueTimes(band).partnerQuanta,
        config.partnerSeq.sequence,
        proposalsGoupedByPartnerAndSortedByRanking(band),
        obsAccessor(band)
      )

    // All we need to construct an empty queue is the band.
    def emptyQueue(band: QueueBand): ProposalQueueBuilder =
      ProposalQueueBuilder(queueTimes(band), band)

    // Building a queue is a state transition.
    def runQueue(band: QueueBand): State[(SemesterResource, ProposalLog), ProposalQueue] =
      State { case (res, log) =>
        val stage = QueueCalcStage(
          queue       = emptyQueue(band),
          iter        = iteratorFor(band),
          activeList  = _.obsListFor(band),
          res         = res,
          log         = log,
        )
        ((stage.resource, stage.log), stage.queue)
      }

    // Run the queues in order!
    val ((finalResource, band123log), (queue1WithoutClassical, queue2, queue3)) = (
      runQueue(QBand1) |@| runQueue(QBand2) |@| runQueue(QBand3)
    ).tupled.run((semesterResource, ProposalLog.Empty))

    // Add classical proposals back to Band 1
    val queue1 = new ProposalQueue {
      def band      = queue1WithoutClassical.band
      def queueTime = queue1WithoutClassical.queueTime
      def toList    = queue1WithoutClassical.toList ++ classicalProps
    }

    // All Band 4 proposals that made it to ITAC are accepted.
    val queue4 = new ProposalQueue {
      def band      = QBand4
      def queueTime = queueTimes(QBand4)
      def toList    = queueProposals(QBand4)
    }

    // Band 4 proposals need to go into the log.
    val band1234log: ProposalLog =
      queue4.toList.foldLeft(band123log)((l, p) => l.updated(p.id, QBand4, AcceptMessage(p)))

    // Removed proposals need to go into the log.
    val finalLog: ProposalLog =
      removed.foldLeft(band1234log)((l, p) => l.updated(p.id, QBand1, RemovedRejectMessage(p)))

    // Assemble our final result for the user
    new QueueCalc {
      val context           = config.binConfig.context
      val proposalLog       = finalLog
      val bucketsAllocation = BucketsAllocationImpl(finalResource.ra.grp.bins.toList)
      def queue(b: QueueBand) =
        b match {
          case QBand1 => queue1
          case QBand2 => queue2
          case QBand3 => queue3
          case QBand4 => queue4
        }
    }

  }








  implicit class ProposalListOps(self: List[Proposal]) {
    def groupByPartnerAndSortedByRanking: Map[Partner, List[Proposal]] =
      self.groupBy(_.ntac.partner).map { case (k, v) => (k, v.sortBy(_.ntac.ranking)) }
  }

  case class RaAllocation(name: String, boundedTime: BoundedTime)
  case class BucketsAllocationImpl(raBins: List[RaResource]) extends BucketsAllocation {

    sealed trait Row extends Product with Serializable
    case class RaRow(h: String, remaining: Double, used: Double, limit: Double) extends Row
    case class ConditionsRow(t: ConditionsCategory, u: Double, r: Double, l: Double) extends Row

    val hPerBin  = 24 / raBins.length
    val binHours = 0 until 24 by 24 / raBins.length
    val raRanges = binHours.map(h => s"$h-${h + hPerBin} h")
    val report = raRanges.zip(raBins).toList.map {
      case (h, b) =>

        val binUsedMins: Double =
          b.condsRes.bins.bins.values.map(_.used.toMinutes.value).sum

        val ra = RaRow(
          h,
          math.round(b.remaining.toMinutes.value) / 60.0,
          math.round(binUsedMins) / 60.0,
          math.round(b.limit.toMinutes.value) / 60.0
        )

        val conds = b.condsRes.bins.bins.toList.sortBy(_._1.name).map {
          case (c, t) =>
            ConditionsRow(
              c,
              math.round(t.used.toMinutes.value) / 60.0,
              (math.round(t.remaining.toMinutes.value) / 60.0) min ra.remaining,
              math.round(t.limit.toMinutes.value) / 60.0
            )
        }

        ra :: conds
    }

    override def toString =
      report.mkString("\n")

    // Annoying, we need to turn off ANSI color if output is being redirected. In the `main` project
    // we have a `Colors` module for this but in `engine` there's no such thing we we'll just hack
    // it in again.
    def embolden(s: String): String =
      if (System.console != null || sys.props.get("force-color").isDefined) s"${Console.BOLD}$s${Console.RESET}"
      else s

    val raTablesANSI: String =
      report.flatten.map {
        case RaRow(h, r, u, l)         => embolden(f"\nRA: $h%-78s  $l%6.2f  $u%6.2f  $r%6.2f")
        case ConditionsRow(t, u, r, l) => f"Conditions: $t%-70s  $l%6.2f  $u%6.2f  $r%6.2f "
      } .mkString("\n")

  }

}


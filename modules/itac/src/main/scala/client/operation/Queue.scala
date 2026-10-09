// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac
package operation

import cats.*
import cats.effect.*
import cats.implicits.*
import edu.gemini.tac.qengine.impl.QueueEngine3
import itac.config.Common
import itac.util.Colors
import lucuma.core.data.PerSite
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import org.typelevel.log4cats.Logger

import java.nio.file.Path
import lucuma.core.enums.ScienceBand
import edu.gemini.tac.qengine.api.QueueCalc
import lucuma.core.model.Semester
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.log.AcceptMessage
import lucuma.core.enums.TimeAccountingCategory
import edu.gemini.tac.qengine.log.RejectTimeAccountingCategoryOverAllocation
import edu.gemini.tac.qengine.log.RejectCategoryOverAllocation
import edu.gemini.tac.qengine.log.RejectTarget
import edu.gemini.tac.qengine.log.RejectConditions
import edu.gemini.tac.qengine.log.RejectOverAllocation
import edu.gemini.tac.qengine.log.RemovedRejectMessage

object Queue {

  /**
    * @param siteConfig path to site-specific configuration file, which can be absolute or relative
    *   (in which case it will be resolved relative to the workspace directory).
    */
  def apply[F[_]: Sync](
    qe:             QueueEngine3.type,
    siteConfig:     PerSite[Path],
  ): Operation[F] =
    new AbstractQueueOperation[F](qe, siteConfig):

      def siteReport(ps: List[Proposal], site: Site, semester: Semester, queueCalc: QueueCalc, cc: Common): F[Unit] =
        Sync[F].delay {          

          val pids = queueCalc.proposalLog.proposalIds // proposals that were considered

          val separator = "━" * 100 + "\n"

          println(s"\n${Colors.BOLD}${site.shortName} ${cc.semester} Queue Candidate${Colors.RESET}")
          println(s"${new java.util.Date}\n") // lazy, this has a reasonable default toString

          println(separator)

          println(s"${Colors.BOLD}RA/Conditions Bucket Allocations:                                                    Limit    Used   Avail${Colors.RESET}")
          println("TODO!!!")
          // println(queueCalc.bucketsAllocation.raTablesANSI)
          println()

          println(separator)

          Enumerated[ScienceBand].all.foreach { qb =>

            println(s"${Colors.BOLD}The following proposals were accepted for Band ${qb.intValue}.${Colors.RESET}")
            println(qb.intValue match {
              case 1 => Colors.YELLOW
              case 2 => Colors.GREEN
              case 3 => Colors.BLUE
              case 4 => Colors.RED
            })

            queueCalc.queues(site)(qb).toList.sortBy(_.parentProposal.ranking.value).foreach: p =>
              println(f"${p.parentProposal.ranking}%5.1f ${p.reference}%-30s ${"todo-pi name"}%-20s ${p.allocation.duration.toHours}%5.1f h  ${"todo: programid"}")

            // result.entries(qb).sortBy(_.proposals.head.ntac.ranking.num.orEmpty).foreach { case QueueResult.Entry(ps, pid) =>
            //   val ss = ps.map { p =>
            //     f"${p.ntac.ranking.num.orEmpty}%5.1f ${p.id.reference}%-30s ${p.piName.orEmpty.take(20)}%-20s ${p.time.toHours.value}%5.1f h  $pid"
            //   }
            //   printWithGroupBars(ss.toList)
            // }
            println(Colors.RESET)
          }

          // println(separator)

          // def hasProposals(p: Partner): Boolean = queueCalc.toList.exists(_.ntac.partner == p)

          // // Partners that appear in the queue
          // Partner.all.sortBy(_.id).filter(hasProposals) foreach { p =>
          //   println(s"${Colors.BOLD}Partner Details for ${if (p.id == "CFH") "GT" else p.toString} ${Colors.RESET}\n")
          //   QueueBand.values.foreach { qb =>
          //     print(qb.number match {
          //       case 1 => Colors.YELLOW
          //       case 2 => Colors.GREEN
          //       case 3 => Colors.BLUE
          //       case 4 => Colors.RED
          //     })
          //     result.entries(qb, p).sortBy(_.proposals.head.ntac.ranking.num.orEmpty).foreach { case QueueResult.Entry(ps, pid) =>
          //       val ss = ps.map { p =>
          //         f"${p.ntac.ranking.num.orEmpty}%5.1f ${p.id.reference}%-30s ${p.piName.orEmpty.take(20)}%-20s ${p.time.toHours.value}%5.1f h  $pid"
          //       }
          //       printWithGroupBars(ss.toList)
          //     }
          //     print(Colors.RESET)
          //     val q = queueCalc.queue(qb)
          //     if (qb.number < 4) {
          //       val used  = q.usedTime(p).toHours.value
          //       val avail = q.queueTime(p).toHours.value
          //       val pct   = if (avail == 0) 0.0 else (used / avail) * 100
          //       println(f"                                                 B${qb.number} Total: $used%5.1f h/${avail}%5.1f h ($pct%3.1f%% ≤ ${(q.queueTime.overfillAllowance.doubleValue + 100.0)}%3.1f%%)")
          //     } else {
          //       val used = q.usedTime(p).toHours.value
          //       println(f"                                                 B${qb.number} Total: $used%5.1f h\n")
          //     }
          //     println()
          //   }
          //   println()
          // }

          println(separator)
          println(s"${Colors.BOLD}Rejection Report for ${site.shortName}-${Semester.fromString.reverseGet(semester)}${Colors.RESET}\n")

          Enumerated[ScienceBand].all.foreach { qb =>
            println(s"${Colors.BOLD}The following proposals were rejected for $qb.${Colors.RESET}")
            pids.toList.flatMap(pid => ps.find(_.reference === pid.parentReference)).sortBy(_.ranking.value).foreach { p =>
              Enumerated[TimeAccountingCategory].all.foreach: cat =>
                val shard = p.shardFor(site, cat, qb)
                if shard.allocation.duration.nonZero then
                  val sid = shard.reference
                  queueCalc.proposalLog.get(sid, qb) match {
                    case None | Some(AcceptMessage(_))            => //println(f"- ${pid.reference}%-20s ${p.piName.orEmpty}%-15s 👍")
                    case Some(m: RejectTimeAccountingCategoryOverAllocation)  => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${"Time accounting category full:"}%-20s ${m.detail}")
                    case Some(m: RejectCategoryOverAllocation) => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${"Category overallocated:"}%-20s ${m.detail}")
                    case Some(m: RejectTarget)                 => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${m.raDecType.toString + " bin full:"}%-20s ${m.detail}}")
                    case Some(m: RejectConditions)             => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${"Conditions bin full:"}%-20s ${m.detail}}")
                    case Some(m: RejectOverAllocation)         => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${"Overallocation"}%-20s ${m.detail}")
                    case Some(m: RemovedRejectMessage)         => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${"Removed"}%-20s ${m.detail}")
                    case Some(lm)                              => println(f"${p.ranking.value}%5.1f ${sid}%-20s ${"todo-pi name"}%-15s ${"Miscellaneous"}%-20s ${lm.getClass.getName}")
                  }
            }
            println()
          }

          // println(s"${Colors.BOLD}The following proposals for ${queueCalc.context.site.abbreviation} do not appear in the proposal log:${Colors.RESET}")
          // ps.foreach { p =>
          //   if (p.site == queueCalc.context.site && QueueBand.values.forall(log.get(p.id, _).isEmpty)) {
          //     println(f"- ${p.id.reference}%-30s ${p.piName.orEmpty}%-20s  ${p.time.toHours.value}%5.1f h (${p.mode})")
          //   }
          // }
          // println()

          // // Find proposals with divided time.
          // val dividedProposals: List[Proposal] =
          //   ps.sortBy(_.id.reference).filter(p => p.time != p.undividedTime && p.site == queueCalc.context.site)

          // val thisSite = queueCalc.context.site
          // val otherSite = if (thisSite == Site.GN) Site.GS else Site.GN

          // if (dividedProposals.nonEmpty) {
          //   println(separator)
          //   println(s"${Colors.BOLD}Time proportions for ${thisSite} proposals that also have awarded time at ${otherSite} ${Colors.RESET}\n")
          //   println(s"${Colors.BOLD}  Reference         PI                      Award     GN     GS${Colors.RESET}")
          //                         //- CA-2020B-013     Drout                   3.8 h    2.0    1.8
          //   dividedProposals.foreach { p =>
          //     val t  = p.undividedTime.toHours.value
          //     val tʹ = p.time.toHours.value
          //     val (gn, gs) = queueCalc.context.site match {
          //       case Site.GN => (tʹ, t - tʹ)
          //       case Site.GS => (t - tʹ, tʹ)
          //     }
          //     println(f"- ${p.id.reference}%-16s  ${p.piName.orEmpty.take(20)}%-20s  $t%5.1f h  $gn%5.1f  $gs%5.1f")
          //   }
          // }


        }


      def run(ws: Workspace[F], log: Logger[F]): F[ExitCode] =
        ws.commonConfig.flatMap: cc =>
          computeQueue(ws).flatMap: (ps, queueCalc) =>
            Enumerated[Site]
              .all
              .traverse: site =>
                siteReport(ps, site, cc.semester, queueCalc, cc)
              .as(ExitCode.Success)

}



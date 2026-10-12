// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api

import edu.gemini.tac.qengine.api.queue.ProposalQueue
import edu.gemini.tac.qengine.log.ProposalLog
import lucuma.core.data.PerSite
import lucuma.core.enums.ScienceBand
import edu.gemini.tac.qengine.impl.resource.PerRightAscensionResource
import edu.gemini.tac.qengine.api.config.ConditionsCategory

/**
 * A queue calculation result.  This is a combination of a ProposalQueue and
 * a ProposalLog.  The queue contains the selected proposals and statistics
 * while the log records what happened to the proposals that were not selected.
 */
case class QueueCalc(
  queues: PerSite[ScienceBand => ProposalQueue],
  proposalLog: ProposalLog,
  bucketsAllocation: PerSite[BucketsAllocation],
  // def toList: List[Proposal] = Enumerated[ScienceBand].all.flatMap(queue(_).toList)
)

case class BucketsAllocation(raBins: List[PerRightAscensionResource]) {

  sealed trait Row extends Product with Serializable
  case class RaRow(h: String, remaining: Double, used: Double, limit: Double) extends Row
  case class ConditionsRow(t: ConditionsCategory, u: Double, r: Double, l: Double) extends Row

  val hPerBin  = 24 / raBins.length
  val binHours = 0 until 24 by 24 / raBins.length
  val raRanges = binHours.map(h => s"$h-${h + hPerBin} h")
  val report = raRanges.zip(raBins).toList.map {
    case (h, b) =>

      val binUsedMins: Double =
        b.condsRes.bins.bins.values.map(_.used.toMinutes.toDouble).sum

      val ra = RaRow(
        h,
        math.round(b.remaining.toMinutes.toDouble) / 60.0,
        math.round(binUsedMins) / 60.0,
        math.round(b.limit.toMinutes.toDouble) / 60.0
      )

      val conds = b.condsRes.bins.bins.toList.sortBy(_._1.name).map {
        case (c, t) =>
          ConditionsRow(
            c,
            math.round(t.used.toMinutes.toDouble) / 60.0,
            (math.round(t.remaining.toMinutes.toDouble) / 60.0) min ra.remaining,
            math.round(t.limit.toMinutes.toDouble) / 60.0
          )
      }
      //s"$ra\n${conds.mkString("\n")}"
      ra :: conds
  }

  override def toString = {
    report.mkString("\n")
    //BucketsAllocationImpl(Nil)
    //System.exit(0)
  }

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

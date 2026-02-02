// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import cats.data.*
import cats.data.Validated
import cats.syntax.all.*
import edu.gemini.tac.qengine.p1.*
import org.slf4j.LoggerFactory

object QueueEngineBandProblems {
  import QueueBand._

  private val Log = LoggerFactory.getLogger("edu.gemini.itac")

  /** A function type that is define only in cases of failure. */
  type Problem = PartialFunction[(Proposal, QueueBand), String]

  val ClassicalNotInBand1: Problem = {
    case (p, b@(QBand2 | QBand3 | QBand4)) if p.mode == Mode.Classical =>
      s"Classical proposal in band ${b.number}"
  }

  val NoObsInBand: Problem = {
    case (p, b) if p.obsListFor(b).isEmpty =>
      s"No observations were found for Band ${b.number}"
  }

  val LpInBand3Or4: Problem = {
    case (p, b@(QBand3 | QBand4)) if p.mode == Mode.LargeProgram =>
      s"LP proposal in Band ${b.number}"
  }

  val RapidTooOutsideBand1: Problem = {
    case (p, b@(QBand2 | QBand3 | QBand4)) if p.too == Too.rapid =>
      s"Rapid TOO proposal in Band ${b.number}"
  }

  val StandardTooOutsideBand12: Problem = {
    case (p, b@(QBand3 | QBand4)) if p.too == Too.standard =>
      s"Standard TOO proposal in band ${b.number}"
  }

  val All: NonEmptyList[Problem] =
    NonEmptyList.of(ClassicalNotInBand1, NoObsInBand, LpInBand3Or4, RapidTooOutsideBand1, StandardTooOutsideBand12)

  def checkAll(p: Proposal, b: QueueBand): ValidatedNel[String, Unit] =
    All.foldMap: problem => 
      problem.lift((p, b)).fold(().valid)(NonEmptyList.one(_).invalid)

  def unsafeCheckAll(p: Proposal, b: QueueBand): Unit =
    checkAll(p, b) match {
      case Validated.Valid(u)  => u
      case Validated.Invalid(es) => es.toList.foreach(w => Log.warn(s"${p.ntac.reference}: $w"))
    }

}
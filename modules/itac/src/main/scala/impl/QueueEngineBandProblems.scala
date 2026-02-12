// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl

import cats.data.*
import cats.data.Validated
import cats.syntax.all.*
import edu.gemini.tac.qengine.p1.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.ToOActivation
import org.slf4j.LoggerFactory

object QueueEngineBandProblems {
  import ScienceBand._

  private val Log = LoggerFactory.getLogger("edu.gemini.itac")

  /** A function type that is define only in cases of failure. */
  type Problem = PartialFunction[(Proposal, ScienceBand), String]

  val ClassicalNotInBand1: Problem = {
    case (p, b@(Band2 | Band3 | Band4)) if p.mode == ScienceSubtype.Classical =>
      s"Classical proposal in $b"
  }

  val NoObsInBand: Problem = {
    case (p, b) if p.obsListFor(b).isEmpty =>
      s"No observations were found for $b"
  }

  val LpInBand3Or4: Problem = {
    case (p, b@(Band3 | Band4)) if p.mode == ScienceSubtype.LargeProgram =>
      s"LP proposal in $b"
  }

  val RapidTooOutsideBand1: Problem = {
    case (p, b@(Band2 | Band3 | Band4)) if p.too == ToOActivation.Rapid =>
      s"Rapid TOO proposal in $b"
  }

  val StandardTooOutsideBand12: Problem = {
    case (p, b@(Band3 | Band4)) if p.too == ToOActivation.Standard =>
      s"Standard TOO proposal in $b"
  }

  val All: NonEmptyList[Problem] =
    NonEmptyList.of(ClassicalNotInBand1, NoObsInBand, LpInBand3Or4, RapidTooOutsideBand1, StandardTooOutsideBand12)

  def checkAll(p: Proposal, b: ScienceBand): ValidatedNel[String, Unit] =
    All.foldMap: problem => 
      problem.lift((p, b)).fold(().valid)(NonEmptyList.one(_).invalid)

  def unsafeCheckAll(p: Proposal, b: ScienceBand): Unit =
    checkAll(p, b) match {
      case Validated.Valid(u)  => u
      case Validated.Invalid(es) => es.toList.foreach(w => Log.warn(s"${p.ntac.reference}: $w"))
    }

}
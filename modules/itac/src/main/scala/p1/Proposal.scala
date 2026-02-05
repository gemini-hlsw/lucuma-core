// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site

import java.io.File

case class Proposal(
  ntac: Ntac,
  site: Site,
  mode: Mode = Mode.Queue,
  too: Too.Value = Too.none,
  obsList: List[Observation] = Nil,
  band3Observations: List[Observation] = Nil,
  isPoorWeather: Boolean = false,
  piName: Option[String] = None,
  piEmail: Option[String] = None,
  p1proposal: Null = null, // edu.gemini.model.p1.immutable.Proposal = null, // to avoid having to generate one for testcases that don't care
  p1mutableProposal: Null = null, // edu.gemini.model.p1.mutable.Proposal = null, // to avoid having to generate one for testcases that don't care
  p1xmlFile: File = null, // to avoid having to generate one for testcases that don't care
  itacComment: Option[String] = None,
) {

  lazy val id: Proposal.Id = Proposal.Id(ntac.partner, ntac.reference)

  def obsListFor(band: QueueBand): List[Observation] =
    if (band == QueueBand.QBand3) band3Observations else obsList

  /**
   * Gets the time for the proposal as a whole.
   */
  def time: Time = ntac.awardedTime

  /**
   * Returns the original awarded time for this proposal, which will be more than `awardedTime` if
   * the original proposal was split in half due to the presence of observations at both sites.
   */
  def undividedTime: Time = ntac.undividedTime.getOrElse(time)

  /**
   * Gets the time for the given observation relative to the total for all
   * observations in the proposal.
   */
  def relativeObsTime(obs: Observation, band: QueueBand): Time =
    Observation.relativeObsTime(obs, time, obsListFor(band))

  /**
   * Gets the observation list with their times adjusted to be relative to
   * the total for all observations in the proposal.
   */
  def relativeObsList(band: QueueBand): List[Observation] =
    Observation.relativeObsList(time, obsListFor(band))

  def p1pdfBaseName: Option[String] =
    Option(p1xmlFile).map { f =>
      val name     = f.getName
      name.lastIndexOf('.') match {
        case -1 => name
        case  n => name.substring(0, n)
      }
    }

  def p1pdfFile: String =
    p1pdfBaseName.map(s => s + ".pdf").orNull

  def p1pdfStage2File: String =
    p1pdfBaseName.map(s => s + "_stage2.pdf").orNull

  def p1pdfs: Proposal.Pdfs[String] =
    Proposal.Pdfs(p1pdfFile, p1pdfStage2File)
}

object Proposal {

  final case class Id(partner: Partner, reference: String)
  object Id {
    implicit val OrderingId: Ordering[Id] =
      Ordering.by(id => (id.partner.tag, id.reference))
  }

  case class Pdfs[A](
    p1pdf: A,
    p1pdfStage2: A
  ) {

    def map[B](f: A => B): Pdfs[B] =
      Pdfs(f(p1pdf), f(p1pdfStage2))

    def toList: List[A] =
      List(p1pdf, p1pdfStage2)

  }

}

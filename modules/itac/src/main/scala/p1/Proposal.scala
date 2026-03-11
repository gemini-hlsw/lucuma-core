// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.ToOActivation
import lucuma.core.util.TimeSpan

case class Proposal(
  ntac: Ntac,
  site: Site,
  mode: ScienceSubtype = ScienceSubtype.Queue,
  too: ToOActivation = ToOActivation.None,
  obsList: List[ItacObservation] = Nil,
  band3Observations: List[ItacObservation] = Nil,
  isPoorWeather: Boolean = false,
  piName: Option[String] = None,
  piEmail: Option[String] = None,
  itacComment: Option[String] = None,
) {

  lazy val id: Proposal.Id = Proposal.Id(ntac.TimeAccountingCategory, ntac.reference)

  def obsListFor(band: ScienceBand): List[ItacObservation] =
    if (band == ScienceBand.Band3) band3Observations else obsList

  /**
   * Gets the time for the proposal as a whole.
   */
  def time: TimeSpan = ntac.awardedTime

  /**
   * Returns the original awarded time for this proposal, which will be more than `awardedTime` if
   * the original proposal was split in half due to the presence of observations at both sites.
   */
  def undividedTime: TimeSpan = ntac.undividedTime.getOrElse(time)

  /**
   * Gets the time for the given observation relative to the total for all
   * observations in the proposal.
   */
  def relativeObsTime(obs: ItacObservation, band: ScienceBand): TimeSpan =
    ItacObservation.relativeObsTime(obs, time, obsListFor(band))

  /**
   * Gets the observation list with their times adjusted to be relative to
   * the total for all observations in the proposal.
   */
  def relativeObsList(band: ScienceBand): List[ItacObservation] =
    ItacObservation.relativeObsList(time, obsListFor(band))

}

object Proposal {

  final case class Id(TimeAccountingCategory: TimeAccountingCategory, reference: String)
  object Id {
    implicit val OrderingId: Ordering[Id] =
      Ordering.by(id => (id.TimeAccountingCategory.tag, id.reference))
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

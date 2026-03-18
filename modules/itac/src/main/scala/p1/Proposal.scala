// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.Site
import lucuma.core.enums.ToOActivation
import lucuma.core.model.ProposalReference
import lucuma.core.util.TimeSpan

case class Proposal(
  reference: ProposalReference,
  ntac: Ntac,
  site: Site,
  mode: ScienceSubtype = ScienceSubtype.Queue,
  too: ToOActivation = ToOActivation.None,
  obsList: List[ItacObservation] = Nil,
  band3Observations: List[ItacObservation] = Nil,
  isPoorWeather: Boolean = false,
) {

  def obsListFor(band: ScienceBand): List[ItacObservation] =
    if (band == ScienceBand.Band3) band3Observations else obsList

  /**
   * Gets the time for the proposal as a whole.
   */
  def time: TimeSpan = ntac.awardedTime

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

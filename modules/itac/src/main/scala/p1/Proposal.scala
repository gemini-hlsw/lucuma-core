// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.enums.ScienceBand
import lucuma.core.enums.ToOActivation
import lucuma.core.model.ProposalReference
import lucuma.core.util.TimeSpan
import cats.syntax.all.*
import cats.data.NonEmptyList
import lucuma.core.model.Allocation
import lucuma.core.model.ProposalType
import lucuma.core.model.IntPercent

case class Proposal(
  reference: ProposalReference,
  allocations: NonEmptyList[Allocation],
  tpe: ProposalType = ProposalType.Queue(ToOActivation.None, IntPercent.unsafeFrom(0), Nil), // TODO
  obsList: List[ItacObservation] = Nil,
  band3Observations: List[ItacObservation] = Nil,
) {

  def too: ToOActivation =
    ProposalType.ToOActivation.getOption(tpe).getOrElse(ToOActivation.None)

  def obsListFor(band: ScienceBand): List[ItacObservation] =
    if (band == ScienceBand.Band3) band3Observations else obsList

  @deprecated
  def ntac = allocations.head

  /**
   * Gets the time for the proposal as a whole.
   */
  def time: TimeSpan = allocations.foldMap(_.duration)

  /**
   * Gets the time for the given observation relative to the total for all
   * observations in the proposal.
   */
  def relativeObsTime(obs: ItacObservation, band: ScienceBand): TimeSpan =
    ItacObservation.relativeObsTime(obs, time, obsListFor(band))

}


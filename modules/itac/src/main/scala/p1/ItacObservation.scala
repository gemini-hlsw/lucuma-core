// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.syntax.all.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.core.data.Metadata
import lucuma.core.util.DateInterval
import lucuma.core.enums.ScienceBand

case class ItacObservation(
  itacTarget: ItacTarget, // TODO: ToO
  constraintSet: ConstraintSet, 
  time: TimeSpan, // estimated time
  lgs: Boolean = false, // TODO: compute this from observing mode?
  mode: ObservingModeType = ObservingModeType.GmosNorthLongSlit,
):

  /** Is the instrument available at the given site, for any amount of time in the given date interval? */
  def isObservableAtSite(site: Site, when: DateInterval)(using Metadata): Boolean =
    !mode.instrument.availability.forSiteAndDateInterval(site, when).isEmpty // TODO: compute available time based on this

  /** Is this observation observable in the specified band? This is determined by conditions and the presence of ToO targets. */
  def isObservableInBand(band: ScienceBand): Boolean =
    true // TODO

object ItacObservation {

  /** An ItacObservation whose `time` field has been scaled to allocated time in a particular band+site. */
  opaque type Scaled <: ItacObservation = ItacObservation
  object Scaled:
    def apply(io: ItacObservation): Scaled = io

  private def percentOfSum(obs: ItacObservation, lst: List[ItacObservation]): Double =
    obs.time.toMilliseconds.toDouble / lst.foldMap(_.time).toMilliseconds.toDouble

  /**
   * Gets the time for the given observation relative to the total for all
   * observations in the proposal.
   */
  def relativeObsTime(obs: ItacObservation, time: TimeSpan, lst: List[ItacObservation]): TimeSpan =
    TimeSpan.fromMillisecondsBounded((percentOfSum(obs, lst) * time.toMilliseconds.toLong).round.toLong)

  /**
   * Gets the observation list with their times adjusted to be relative to
   * the total for all observations in the proposal.
   */
  def relativeObsList(time: TimeSpan, lst: List[ItacObservation]): List[ItacObservation] =
    lst map { obs => obs.copy(time = relativeObsTime(obs, time, lst)) }

}
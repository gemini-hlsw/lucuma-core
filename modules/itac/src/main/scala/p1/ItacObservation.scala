// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan
import lucuma.core.enums.Site
import lucuma.core.enums.ObservingModeType

case class ItacObservation(
  itacTarget: ItacTarget, 
  constraintSet: ConstraintSet, 
  time: TimeSpan, // estimated time
  lgs: Boolean = false, // TODO: compute this from observing mode?
  mode: ObservingModeType = ObservingModeType.GmosNorthLongSlit,
):
  def site: Site = ??? // we need to know this because some partners can't observe 


object ItacObservation {

  /** An ItacObservation whose `time` field has been scaled to allocated time in a particular band+site. */
  opaque type Scaled <: ItacObservation = ItacObservation
  object Scaled:
    def apply(io: ItacObservation): Scaled = io

  def sumObsTime(lst: List[ItacObservation]): TimeSpan = lst.foldLeft(TimeSpan.Zero)(_ +| _.time)

  private def percentOfSum(obs: ItacObservation, lst: List[ItacObservation]): Double =
    obs.time.toMilliseconds.toDouble / sumObsTime(lst).toMilliseconds.toDouble

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
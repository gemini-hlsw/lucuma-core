// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.model.ConstraintSet
import lucuma.core.util.TimeSpan

case class ItacObservation(
  itacTarget: ItacTarget, 
  constraintSet: ConstraintSet, 
  time: TimeSpan, 
  lgs: Boolean = false
)

object ItacObservation {
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
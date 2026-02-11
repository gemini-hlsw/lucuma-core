// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import edu.gemini.tac.qengine.util.Time

case class Observation(
  target: Target, 
  conditions: ObservingConditions, 
  time: Time, 
  lgs: Boolean = false
)

object Observation {
  def sumObsTime(lst: List[Observation]): Time = lst.foldLeft(Time.Zero)(_ + _.time)

  private def percentOfSum(obs: Observation, lst: List[Observation]): Double =
    obs.time.ms / sumObsTime(lst).ms.toDouble

  /**
   * Gets the time for the given observation relative to the total for all
   * observations in the proposal.
   */
  def relativeObsTime(obs: Observation, time: Time, lst: List[Observation]): Time =
    Time.millisecs((percentOfSum(obs, lst) * time.ms).round.toLong).to(obs.time.unit)

  /**
   * Gets the observation list with their times adjusted to be relative to
   * the total for all observations in the proposal.
   */
  def relativeObsList(time: Time, lst: List[Observation]): List[Observation] =
    lst map { obs => obs.copy(time = relativeObsTime(obs, time, lst)) }

}
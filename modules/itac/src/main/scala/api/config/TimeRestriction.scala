// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Observation
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qengine.p1.QueueBand
import edu.gemini.tac.qengine.p1.WaterVapor
import edu.gemini.tac.qengine.p1.WaterVapor.WV50
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time

import scala.Ordering.Implicits.*

/**
 * TimeRestriction associates a name, a value, and a predicate.  The value
 * specifies the time being restricted, which may be an absolute amount of time
 * or a relative amount of time.
 */
case class TimeRestriction[T](name: String, value: T)(val matches: (Proposal, Observation, QueueBand) => Boolean) {

  def map[U](f: T => U): TimeRestriction[U]    =
    new TimeRestriction[U](name, f(value))(matches)

  def updated(newValue: T): TimeRestriction[T] =
    new TimeRestriction[T](name, newValue)(matches)

}

object TimeRestriction {
  def wv(limit: Percent): TimeRestriction[Percent] = wv(limit, WV50)

  def wv(limit: Percent, wv: WaterVapor) =
    TimeRestriction("WV Queue Time Limit", limit) {
       (_, obs, _) => obs.conditions.wv <= wv
    }

  def lgs(limit: Time) =
    TimeRestriction("LGS Queue Time Limit", limit) {
      (_, obs, _) => obs.lgs
    }


}
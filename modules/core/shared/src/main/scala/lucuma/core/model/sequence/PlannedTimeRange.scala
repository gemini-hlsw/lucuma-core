// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.order.*

/**
 * A (PlannedTime, PlannedTime) tuple with a guaranteed order (min <= max).
 */
sealed abstract case class PlannedTimeRange(min: PlannedTime, max: PlannedTime)

object PlannedTimeRange {

  /**
   * Creates a PlannedTimeRange from a single PlannedTime, which becomes both
   * the min and max of the range.
   */
  def single(p: PlannedTime): PlannedTimeRange =
    new PlannedTimeRange(p, p) {}

  /**
   * Creates a PlannedTimeRange from two PlannedTime values in arbitrary order,
   * assigning the minimum one to `min` and the maximum one to `max`.
   */
  def from(p0: PlannedTime, p1: PlannedTime): PlannedTimeRange =
    if (p0 <= p1) new PlannedTimeRange(p0, p1) {} else new PlannedTimeRange(p1, p0) {}

  given Eq[PlannedTimeRange] =
    Eq.by(a => (a.min, a.max))

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.order.*
import monocle.Iso

/**
 * A (PlannedTime, PlannedTime) tuple with a guaranteed order (min <= max).
 * @deprecated("replaced with CategorizedTimeRange", "0.90.0")
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

  import PlannedTime.ToCategorizedTime

  def ToCategorizedTimeRange: Iso[PlannedTimeRange, CategorizedTimeRange] =
    Iso[PlannedTimeRange, CategorizedTimeRange] { ptr =>
      CategorizedTimeRange.from(ToCategorizedTime.get(ptr.min), ToCategorizedTime.get(ptr.max))
    } { tcr =>
      PlannedTimeRange.from(ToCategorizedTime.reverseGet(tcr.min), ToCategorizedTime.reverseGet(tcr.max))
    }

  given Eq[PlannedTimeRange] =
    Eq.by(a => (a.min, a.max))

}
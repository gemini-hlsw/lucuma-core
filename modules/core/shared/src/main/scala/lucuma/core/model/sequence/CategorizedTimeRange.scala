// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.order.*

/**
 * A (CategorizedTime, CategorizedTime) tuple with a guaranteed order (min <= max).
 */
sealed abstract case class CategorizedTimeRange(min: CategorizedTime, max: CategorizedTime)

object CategorizedTimeRange {

  /**
   * Creates a CategorizedTimeRange from a single CategorizedTime, which becomes both
   * the min and max of the range.
   */
  def single(p: CategorizedTime): CategorizedTimeRange =
    new CategorizedTimeRange(p, p) {}

  /**
   * Creates a CategorizedTimeRange from two CategorizedTime values in
   * arbitrary order, assigning the minimum one to `min` and the maximum one
   * to `max`.
   */
  def from(p0: CategorizedTime, p1: CategorizedTime): CategorizedTimeRange =
    if (p0 <= p1) new CategorizedTimeRange(p0, p1) {} else new CategorizedTimeRange(p1, p0) {}

  given Eq[CategorizedTimeRange] =
    Eq.by(a => (a.min, a.max))

}
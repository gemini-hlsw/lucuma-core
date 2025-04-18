// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.data.NonEmptyList
import lucuma.core.data.Zipper
import lucuma.core.util.TimeSpan

/**
 * Time required for a particular configuration change, as
 * indicated by name and description.
 */
case class ConfigChangeEstimate(
  name:        String,
  description: String,
  estimate:    TimeSpan
)

object ConfigChangeEstimate {

  /**
   * Order primarily by the time estimate itself.
   */
  given Order[ConfigChangeEstimate] =
    Order.by { e => (
      e.estimate,
      e.name,
      e.description
    )}

  /**
   * Selects the maximum ConfigChangeEstimate from a list, keeping up with all
   * the remaining items as well.  Multiple config changes happen in parallel,
   * so the maximum cost is the cost for the lot of them.
   */
  def zipMax(estimates: NonEmptyList[ConfigChangeEstimate]): Zipper[ConfigChangeEstimate] =
    Zipper.fromNel(estimates).focusMax

}


// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.data.Zipper
import lucuma.core.util.TimeSpan

/**
 * Time required for an individual detector to produce its datasets for a given
 * step.  Many instruments (like GMOS-N and GMOS-S) have a single detector that
 * produces a single dataset per step.  Newer instruments (like GHOST's blue
 * red detectors) have multiple detectors and each may produce multiple
 * datasets.
 */
case class DetectorEstimate(
  name:        String,
  description: String,
  dataset:     DatasetEstimate,
  count:       NonNegInt
) {

  /**
   * Time for this detector to produce `count` datasets.
   */
  lazy val estimate: TimeSpan =
    dataset.estimate *| count.value

}

object DetectorEstimate {

  def zero(name: String, description: String): DetectorEstimate =
    DetectorEstimate(name, description, DatasetEstimate.Zero, NonNegInt.unsafeFrom(0))

  given Order[DetectorEstimate] =
    Order.by { e => (
      e.dataset,
      e.count.value,
      e.name,
      e.description
    )}

  /**
   * Selects the maximum DetectorEstimate from a list, keeping up with all
   * the remaining items as well.  When there are multiple detectors in use,
   * the time for the step as a whole is the time for the one that takes the
   * longest to produce all its datasets.
   */
  def zipMax(estimates: NonEmptyList[DetectorEstimate]): Zipper[DetectorEstimate] =
    Zipper.fromNel(estimates).focusMax

}
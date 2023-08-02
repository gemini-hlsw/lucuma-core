// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import lucuma.core.data.Zipper
import lucuma.core.util.TimeSpan

/**
 * Step planned time estimate, including configuration change costs and the exposure time and detector read/write
 * overheads.  Configuration changes happen in parallel, so only the cost of the longest change contributes to the
 * total cost for the step.  Similarly if the instrument has multiple detectors then they work in parallel so only
 * one (the one that takes the most time) contributes to the total step cost.
 *
 * @param configChange estimated configuration change costs, where the Zipper focus is intended to be the maximum cost
 * @param detector estimated exposure time and detector overheads, where the Zipper focus is intended to be the maximum cost
 */
case class StepEstimate(
  configChange: Option[Zipper[ConfigChangeEstimate]],
  detector:     Option[Zipper[DetectorEstimate]]
) {

  /**
   * Total time estimate for the selected configuration change (if any) and
   * the selected detector estimate (if any).
   */
  lazy val total: TimeSpan =
    configChange.foldMap(_.focus.estimate) |+| detector.foldMap(_.focus.estimate)

}

object StepEstimate {

  /**
   * Zero time `StepEstimate`.
   */
  val Zero: StepEstimate =
    StepEstimate(None, None)

  /**
   * Constructs from unordered lists of individual configuration change costs and detector estimates.  The longest
   * estimate in each case contributes to the total `StepEstimate` and the remaining values are for informational
   * purposes only.
   *
   * @param configChange all configuration change estimates
   * @param detector all detector estimates
   */
  def fromMax(
    configChange: List[ConfigChangeEstimate],
    detector:     List[DetectorEstimate]
  ): StepEstimate =
    StepEstimate(
      Zipper.fromList(configChange).map(_.focusMax),
      Zipper.fromList(detector).map(_.focusMax)
    )

  given Eq[StepEstimate] =
    Eq.by { a => (
      a.configChange,
      a.detector
    )}

}

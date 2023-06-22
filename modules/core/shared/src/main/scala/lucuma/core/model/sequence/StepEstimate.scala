// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.foldable.*
import cats.syntax.monoid.*
import lucuma.core.data.Zipper
import lucuma.core.util.TimeSpan

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

  val Zero: StepEstimate =
    StepEstimate(None, None)

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

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

/**
 * Exposure settings for one SCORPIO science detector within a step.
 */
final case class ScorpioDetector(
  exposureTime:  TimeSpan,
  exposureCount: PosInt
) derives Eq:

  def totalExposureTime: TimeSpan =
    exposureTime *| exposureCount.value

object ScorpioDetector:
  /** @group Optics */
  val exposureTime: Lens[ScorpioDetector, TimeSpan] =
    Focus[ScorpioDetector](_.exposureTime)

  /** @group Optics */
  val exposureCount: Lens[ScorpioDetector, PosInt] =
    Focus[ScorpioDetector](_.exposureCount)

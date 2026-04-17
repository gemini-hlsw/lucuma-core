// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan

final case class GhostDetector(
  exposureTime:   TimeSpan,
  exposureCount:  PosInt,
  binning:        GhostBinning,
  readMode:       GhostReadMode
):

  /**
   * Single `exposureTime` multiplied by the `exposureCount`.
   */
  def totalExposureTime: TimeSpan =
    exposureTime *| exposureCount.value

  def asRed: GhostDetector.Red =
    GhostDetector.Red(this)

  def asBlue: GhostDetector.Blue =
    GhostDetector.Blue(this)

object GhostDetector:
  given Eq[GhostDetector] =
    Eq.by: a =>
      (
        a.exposureTime,
        a.exposureCount,
        a.binning,
        a.readMode
      )

  object Red extends NewType[GhostDetector]
  type Red = Red.Type

  object Blue extends NewType[GhostDetector]
  type Blue = Blue.Type
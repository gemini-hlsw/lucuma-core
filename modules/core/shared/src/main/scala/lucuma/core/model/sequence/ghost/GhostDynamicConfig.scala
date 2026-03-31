// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import cats.syntax.order.*
import lucuma.core.util.TimeSpan

final case class GhostDynamicConfig(
  redCamera:         GhostDetector.Red,
  blueCamera:        GhostDetector.Blue,
  ifu1FiberAgitator: Ifu1FiberAgitator = Ifu1FiberAgitator.Disabled,
  ifu2FiberAgitator: Ifu2FiberAgitator = Ifu2FiberAgitator.Disabled
):
  /** Expected overall exposure time for all exposures of either camera. */
  def grossExposureTime: TimeSpan =
    redCamera.value.grossExposureTime max blueCamera.value.grossExposureTime

object GhostDynamicConfig:
  given Eq[GhostDynamicConfig] =
    Eq.by: a =>
      (
        a.redCamera,
        a.blueCamera,
        a.ifu1FiberAgitator,
        a.ifu2FiberAgitator
      )
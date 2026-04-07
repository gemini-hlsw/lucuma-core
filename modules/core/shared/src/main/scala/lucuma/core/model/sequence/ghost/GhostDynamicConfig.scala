// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq

final case class GhostDynamicConfig(
  redCamera:         GhostDetector.Red,
  blueCamera:        GhostDetector.Blue,
  ifu1FiberAgitator: GhostIfu1FiberAgitator = GhostIfu1FiberAgitator.Disabled,
  ifu2FiberAgitator: GhostIfu2FiberAgitator = GhostIfu2FiberAgitator.Disabled
)

object GhostDynamicConfig:
  given Eq[GhostDynamicConfig] =
    Eq.by: a =>
      (
        a.redCamera,
        a.blueCamera,
        a.ifu1FiberAgitator,
        a.ifu2FiberAgitator
      )
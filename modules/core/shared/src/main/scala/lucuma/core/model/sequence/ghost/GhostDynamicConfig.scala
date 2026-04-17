// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import cats.syntax.order.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.util.TimeSpan

final case class GhostDynamicConfig(
  red:               GhostDetector.Red,
  blue:              GhostDetector.Blue,
  ifu1FiberAgitator: GhostIfu1FiberAgitator = GhostIfu1FiberAgitator.Disabled,
  ifu2FiberAgitator: GhostIfu2FiberAgitator = GhostIfu2FiberAgitator.Disabled
):
  /**
   * Textual representation of the red and blue channel exposure time and count
   */
  def description: NonEmptyString =
    NonEmptyString.unsafeFrom:
      s"GHOST red(${red.value.description}) blue(${blue.value.description})"

  /**
   * Total exposure time for the configuration as a whole (max of red and blue
   * total times).
   */
  def totalExposureTime: TimeSpan =
    red.value.totalExposureTime max blue.value.totalExposureTime

object GhostDynamicConfig:
  given Eq[GhostDynamicConfig] =
    Eq.by: a =>
      (
        a.red,
        a.blue,
        a.ifu1FiberAgitator,
        a.ifu2FiberAgitator
      )
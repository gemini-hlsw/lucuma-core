// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.PosAngleConstraint
import lucuma.core.util.Timestamp

/**
 * The mapping contgext is a collection of data required to derive the
 * GhostIfuMapping or validate that a GHOST observation has a mapping.
 */
case class IfuMappingContext(
  resolutionMode:     GhostResolutionMode,
  sky:                Option[Coordinates],
  posAngleConstraint: PosAngleConstraint,
  explicitBase:       Option[Coordinates],
  when:               Timestamp
):
  // Maybe we should limit GHOST to Fixed?
  val angle: Option[Angle] =
    PosAngleConstraint.angle.getOption(posAngleConstraint)

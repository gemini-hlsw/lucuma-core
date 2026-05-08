// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.util.TimeSpan

final case class GhostStaticConfig(
  resolutionMode:                GhostResolutionMode,
  ifuMapping:                    Option[GhostIfuMapping],
  slitViewingCameraExposureTime: Option[TimeSpan]
) derives Eq
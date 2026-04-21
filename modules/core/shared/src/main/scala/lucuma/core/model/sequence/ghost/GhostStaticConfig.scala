// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.util.TimeSpan

// Will the target mode (single vs dual, which if any is sky, etc.) go here?

final case class GhostStaticConfig(
  resolutionMode:                GhostResolutionMode,
  slitViewingCameraExposureTime: Option[TimeSpan]
) derives Eq
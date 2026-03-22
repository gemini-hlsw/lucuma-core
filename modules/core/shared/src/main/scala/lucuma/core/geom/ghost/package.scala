// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.ghost

import lucuma.core.math.syntax.units.*
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*
import lucuma.core.math.Angle

val GhostPlateScale: PlateScale = BigDecimal(0.61).plateScale

val FovDiameter: Angle = 444.arcsec

object all
  extends GhostScienceAreaGeometry
  with GhostIfuPatrolField

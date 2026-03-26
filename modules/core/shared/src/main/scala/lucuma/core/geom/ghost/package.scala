// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.ghost

import lucuma.core.math.Angle
import lucuma.core.math.syntax.int.*
import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.*

val GhostPlateScale: PlateScale = BigDecimal(1.0 / 0.61).plateScale

val FovDiameter: Angle = 444.arcsec

// 1 mm in arcsec at GHOST plate scale (1/0.61 arcsec/mm)
val IFUSeparation: Angle = Angle.fromBigDecimalArcseconds(GhostPlateScale.value)

// 2 mm separation used for HR sky fiber and IFU boundary offsets
val HRFiberOffset: Angle = IFUSeparation + IFUSeparation

object all
  extends GhostScienceAreaGeometry
  with GhostIfuPatrolField

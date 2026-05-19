// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.visitors

import lucuma.core.math.Angle
import lucuma.core.math.syntax.int.*

/**
 * The fixed value for the Alopeke science fov in speckle mode.
 */
val AlopekeSpeckleScienceFov: Angle = 3.arcsec

/**
 * The fixed value for the Alopeke science fov in wide field mode.
 */
val AlopekeWideFieldScienceFov: Angle = 35.arcsec

/**
 * The fixed value for the Zorro science fov in speckle mode.
 */
val ZorroSpeckleScienceFov: Angle = 3.arcsec

/**
 * The fixed value for the Zorro science fov in wide field mode.
 */
val ZorroWideFieldScienceFov: Angle = 35.arcsec

/**
 * The fixed value for the Maroon-X science fov. It is an octogonal IFU with 0.77" diameter.
 */
val MaroonXScienceFov: Angle = 770.milliarcseconds

/**
 * Requested that the probe should not vignette anything inside r=30 arcsec.
 */
val MaroonXSkyFiberPatrol: Angle = 60.arcsec

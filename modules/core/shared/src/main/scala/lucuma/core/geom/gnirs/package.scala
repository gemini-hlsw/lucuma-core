// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gnirs

import lucuma.core.math.Angle
import lucuma.core.math.syntax.int.*

// Long slit lengths derived from ocs.
val SlitLengthShortCamNoXd: Angle = 99.arcsec
val SlitLengthLongCamNoXd:  Angle = 49.arcsec
val SlitLengthShortCamXd:   Angle = 7.arcsec
val SlitLengthLongCamSxd:   Angle = 7.arcsec
val SlitLengthLongCamLxd:   Angle = 5100.mas

// Pinhole sizes
val Pinhole1Size: Angle = 100.mas
val Pinhole3Size: Angle = 300.mas

object all extends GnirsScienceAreaGeometry

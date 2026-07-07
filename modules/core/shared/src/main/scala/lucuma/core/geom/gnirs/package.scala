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

// IFU science-area heights, derived from ocs (InstGNIRS.getScienceArea). The
// width is the IFU "slit width" (GnirsFpuIfu.slitWidth: 3.15" / 1.25").
val IfuLowResHeight:  Angle = 4800.mas
val IfuHighResHeight: Angle = 1800.mas

// Pinhole sizes
val Pinhole1Size: Angle = 100.mas
val Pinhole3Size: Angle = 300.mas

// GNIRS imaging science area (the "keyhole"): a rectangular bar with a circular cap
// on top. The shape depends on the filter; the bar length also depends on the camera.
//
// Order-blocking, narrow-band and H-MK filters see the full keyhole:
//   bar: no-cross-dispersion slit length (99" short cam / 49" long cam) by 10" tall
//   cap: 28" wide, rising 10" above the bar (r ~ 15")
val KeyholeBarHeight:  Angle = 10.arcsec
val KeyholeCapWidth:   Angle = 28.arcsec
val KeyholeCapHeight:  Angle = 10.arcsec
//
// Y-MK, J-MK and K-MK filters see the smaller, round unvignetted field
// (camera-independent):
//   bar: 24" wide by 9" tall
//   cap: 24" wide, rising 7" above the bar (r ~ 12")
val RoundFieldWidth:   Angle = 24.arcsec
val RoundFieldHeight:  Angle = 9.arcsec
val RoundCapWidth:     Angle = 24.arcsec
val RoundCapHeight:    Angle = 7.arcsec

object all extends GnirsScienceAreaGeometry

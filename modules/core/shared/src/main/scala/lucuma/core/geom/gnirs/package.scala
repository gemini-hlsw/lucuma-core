// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gnirs

import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.math.Angle
import lucuma.core.math.syntax.int.*

// Long slit lengths and IFU science-area heights now live beside the FPU
// enumerations, so the drawn shape and the aperture extent cannot drift apart.
val SlitLengthShortCamNoXd: Angle = GnirsFpuSlit.SlitLengthShortCamNoXd
val SlitLengthLongCamNoXd:  Angle = GnirsFpuSlit.SlitLengthLongCamNoXd
val SlitLengthShortCamXd:   Angle = GnirsFpuSlit.SlitLengthShortCamXd
val SlitLengthLongCamSxd:   Angle = GnirsFpuSlit.SlitLengthLongCamSxd
val SlitLengthLongCamLxd:   Angle = GnirsFpuSlit.SlitLengthLongCamLxd

// The IFU width is the IFU "slit width" (GnirsFpuIfu.slitWidth: 3.15" / 1.25").
val IfuLowResHeight:  Angle = GnirsFpuIfu.IfuLowResHeight
val IfuHighResHeight: Angle = GnirsFpuIfu.IfuHighResHeight

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

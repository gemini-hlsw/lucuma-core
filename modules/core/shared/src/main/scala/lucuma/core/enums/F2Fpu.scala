// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import coulomb.*
import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.Pixels
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 focal plane units.
 * @group Enumerations (Generated)
 */
enum F2Fpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Quantity[Int, Pixels],
  val decker: F2Decker
) derives Enumerated:

  case Pinhole       extends F2Fpu("Pinhole",       "Pinhole",         "2-Pixel Pinhole Grid", 0.pixels, F2Decker.Imaging)
  case SubPixPinhole extends F2Fpu("SubPixPinhole", "Sub-Pix Pinhole", "Sub-Pixel Pinhole Gr", 0.pixels, F2Decker.Imaging)
  case LongSlit1     extends F2Fpu("LongSlit_1",    "Long Slit 1px",   "1-Pixel Long Slit",    1.pixels, F2Decker.LongSlit)
  case LongSlit2     extends F2Fpu("LongSlit_2",    "Long Slit 2px",   "2-Pixel Long Slit",    2.pixels, F2Decker.LongSlit)
  case LongSlit3     extends F2Fpu("LongSlit_3",    "Long Slit 3px",   "3-Pixel Long Slit",    3.pixels, F2Decker.LongSlit)
  case LongSlit4     extends F2Fpu("LongSlit_4",    "Long Slit 4px",   "4-Pixel Long Slit",    4.pixels, F2Decker.LongSlit)
  case LongSlit6     extends F2Fpu("LongSlit_6",    "Long Slit 6px",   "6-Pixel Long Slit",    6.pixels, F2Decker.LongSlit)
  case LongSlit8     extends F2Fpu("LongSlit_8",    "Long Slit 8px",   "8-Pixel Long Slit",    8.pixels, F2Decker.LongSlit)

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
enum Flamingos2Fpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Quantity[Int, Pixels],
  val decker: Flamingos2Decker
) derives Enumerated:

  case Pinhole       extends Flamingos2Fpu("Pinhole",       "Pinhole",         "2-Pixel Pinhole Grid", 0.pixels, Flamingos2Decker.Imaging)
  case SubPixPinhole extends Flamingos2Fpu("SubPixPinhole", "Sub-Pix Pinhole", "Sub-Pixel Pinhole Gr", 0.pixels, Flamingos2Decker.Imaging)
  case LongSlit1     extends Flamingos2Fpu("LongSlit_1",    "Long Slit 1px",   "1-Pixel Long Slit",    1.pixels, Flamingos2Decker.LongSlit)
  case LongSlit2     extends Flamingos2Fpu("LongSlit_2",    "Long Slit 2px",   "2-Pixel Long Slit",    2.pixels, Flamingos2Decker.LongSlit)
  case LongSlit3     extends Flamingos2Fpu("LongSlit_3",    "Long Slit 3px",   "3-Pixel Long Slit",    3.pixels, Flamingos2Decker.LongSlit)
  case LongSlit4     extends Flamingos2Fpu("LongSlit_4",    "Long Slit 4px",   "4-Pixel Long Slit",    4.pixels, Flamingos2Decker.LongSlit)
  case LongSlit6     extends Flamingos2Fpu("LongSlit_6",    "Long Slit 6px",   "6-Pixel Long Slit",    6.pixels, Flamingos2Decker.LongSlit)
  case LongSlit8     extends Flamingos2Fpu("LongSlit_8",    "Long Slit 8px",   "8-Pixel Long Slit",    8.pixels, Flamingos2Decker.LongSlit)

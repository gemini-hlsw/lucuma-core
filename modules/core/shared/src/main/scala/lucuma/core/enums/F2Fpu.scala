// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 focal plane units.
 * @group Enumerations (Generated)
 */
enum F2Fpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Int, // pixels
  val decker: F2Decker,
  val obsolete: Boolean
) derives Enumerated:

  case Pinhole       extends F2Fpu("Pinhole",       "Pinhole",         "2-Pixel Pinhole Grid", 0, F2Decker.Imaging, false)
  case SubPixPinhole extends F2Fpu("SubPixPinhole", "Sub-Pix Pinhole", "Sub-Pixel Pinhole Gr", 0, F2Decker.Imaging, false)
  case LongSlit1     extends F2Fpu("LongSlit1",     "Long Slit 1px",   "1-Pixel Long Slit",    1, F2Decker.LongSlit, false)
  case LongSlit2     extends F2Fpu("LongSlit2",     "Long Slit 2px",   "2-Pixel Long Slit",    2, F2Decker.LongSlit, false)
  case LongSlit3     extends F2Fpu("LongSlit3",     "Long Slit 3px",   "3-Pixel Long Slit",    3, F2Decker.LongSlit, false)
  case LongSlit4     extends F2Fpu("LongSlit4",     "Long Slit 4px",   "4-Pixel Long Slit",    4, F2Decker.LongSlit, false)
  case LongSlit6     extends F2Fpu("LongSlit6",     "Long Slit 6px",   "6-Pixel Long Slit",    6, F2Decker.LongSlit, false)
  case LongSlit8     extends F2Fpu("LongSlit8",     "Long Slit 8px",   "8-Pixel Long Slit",    8, F2Decker.LongSlit, false)

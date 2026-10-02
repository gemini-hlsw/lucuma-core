// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import coulomb.*
import lucuma.core.math.Angle
import lucuma.core.math.syntax.units.*
import lucuma.core.math.units.Pixels
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for SCORPIO long-slit focal plane units.
 *
 * Slit widths come from Table 4-5, with the pixel count at the SCORPIO plate scale (0.18"/pix).
 * The 4.32" slit is meant for slitless spectroscopy or as a field stop; its resolution is set by
 * the target size rather than the slit.
 * @group Enumerations
 */
enum ScorpioFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Angle,
  val slitWidthPixels: Quantity[Int, Pixels]
) derives Enumerated, Display:
  case LongSlit_0_36 extends ScorpioFpu("LongSlit_0_36", "0.36\"", "Longslit 0.36 arcsec", Angle.milliarcseconds.reverseGet( 360),  2.pixels)
  case LongSlit_0_54 extends ScorpioFpu("LongSlit_0_54", "0.54\"", "Longslit 0.54 arcsec", Angle.milliarcseconds.reverseGet( 540),  3.pixels)
  case LongSlit_0_72 extends ScorpioFpu("LongSlit_0_72", "0.72\"", "Longslit 0.72 arcsec", Angle.milliarcseconds.reverseGet( 720),  4.pixels)
  case LongSlit_1_08 extends ScorpioFpu("LongSlit_1_08", "1.08\"", "Longslit 1.08 arcsec", Angle.milliarcseconds.reverseGet(1080),  6.pixels)
  case LongSlit_1_44 extends ScorpioFpu("LongSlit_1_44", "1.44\"", "Longslit 1.44 arcsec", Angle.milliarcseconds.reverseGet(1440),  8.pixels)
  case LongSlit_2_16 extends ScorpioFpu("LongSlit_2_16", "2.16\"", "Longslit 2.16 arcsec", Angle.milliarcseconds.reverseGet(2160), 12.pixels)
  case LongSlit_4_32 extends ScorpioFpu("LongSlit_4_32", "4.32\"", "Longslit 4.32 arcsec", Angle.milliarcseconds.reverseGet(4320), 24.pixels)

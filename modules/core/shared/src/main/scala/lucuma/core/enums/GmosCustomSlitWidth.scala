// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq.*
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS custom slit width.
 *
 * The custom slit widths and the builtin long slit FPUs are in 1:1
 * correspondence, so a custom mask may be treated as the builtin long slit of
 * the same width where only the slit width matters (e.g., when calibrating).
 *
 * @param northFpu builtin GMOS North long slit FPU of the same width
 * @param southFpu builtin GMOS South long slit FPU of the same width
 * @group Enumerations
 */
enum GmosCustomSlitWidth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val width: Angle,
  val northFpu: GmosNorthFpu,
  val southFpu: GmosSouthFpu
) derives Enumerated:

  case CustomWidth_0_25 extends GmosCustomSlitWidth("CustomWidth_0_25", "0.25\"", "0.25 arcsec", Angle.fromDoubleArcseconds(0.25), GmosNorthFpu.LongSlit_0_25, GmosSouthFpu.LongSlit_0_25)
  case CustomWidth_0_50 extends GmosCustomSlitWidth("CustomWidth_0_50", "0.50\"", "0.50 arcsec", Angle.fromDoubleArcseconds(0.50), GmosNorthFpu.LongSlit_0_50, GmosSouthFpu.LongSlit_0_50)
  case CustomWidth_0_75 extends GmosCustomSlitWidth("CustomWidth_0_75", "0.75\"", "0.75 arcsec", Angle.fromDoubleArcseconds(0.75), GmosNorthFpu.LongSlit_0_75, GmosSouthFpu.LongSlit_0_75)
  case CustomWidth_1_00 extends GmosCustomSlitWidth("CustomWidth_1_00", "1.00\"", "1.00 arcsec", Angle.fromDoubleArcseconds(1.00), GmosNorthFpu.LongSlit_1_00, GmosSouthFpu.LongSlit_1_00)
  case CustomWidth_1_50 extends GmosCustomSlitWidth("CustomWidth_1_50", "1.50\"", "1.50 arcsec", Angle.fromDoubleArcseconds(1.50), GmosNorthFpu.LongSlit_1_50, GmosSouthFpu.LongSlit_1_50)
  case CustomWidth_2_00 extends GmosCustomSlitWidth("CustomWidth_2_00", "2.00\"", "2.00 arcsec", Angle.fromDoubleArcseconds(2.00), GmosNorthFpu.LongSlit_2_00, GmosSouthFpu.LongSlit_2_00)
  case CustomWidth_5_00 extends GmosCustomSlitWidth("CustomWidth_5_00", "5.00\"", "5.00 arcsec", Angle.fromDoubleArcseconds(5.00), GmosNorthFpu.LongSlit_5_00, GmosSouthFpu.LongSlit_5_00)

object GmosCustomSlitWidth:

  def fromWidth(slitWidth: Angle): Option[GmosCustomSlitWidth] =
    values.find(_.width === slitWidth)

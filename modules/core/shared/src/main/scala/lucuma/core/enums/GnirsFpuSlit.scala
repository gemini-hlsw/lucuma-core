// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.math.Angle
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS FPU Slit.
 * @group Enumerations (Generated)
 */
enum GnirsFpuSlit(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Angle,
) derives Enumerated, Display:
  case LongSlit_0_10 extends GnirsFpuSlit("LongSlit_0_10", "0.10\"", "0.10 arcsec", Angle.fromDoubleArcseconds(0.100))
  case LongSlit_0_15 extends GnirsFpuSlit("LongSlit_0_15", "0.15\"", "0.15 arcsec", Angle.fromDoubleArcseconds(0.150))
  case LongSlit_0_20 extends GnirsFpuSlit("LongSlit_0_20", "0.20\"", "0.20 arcsec", Angle.fromDoubleArcseconds(0.200))
  case LongSlit_0_30 extends GnirsFpuSlit("LongSlit_0_30", "0.30\"", "0.30 arcsec", Angle.fromDoubleArcseconds(0.300))
  case LongSlit_0_45 extends GnirsFpuSlit("LongSlit_0_45", "0.45\"", "0.45 arcsec", Angle.fromDoubleArcseconds(0.450))
  case LongSlit_0_675 extends GnirsFpuSlit("LongSlit_0_675", "0.675\"", "0.675 arcsec", Angle.fromDoubleArcseconds(0.675))
  case LongSlit_1_00 extends GnirsFpuSlit("LongSlit_1_00", "1.0\"", "1.0 arcsec", Angle.fromDoubleArcseconds(1.000))

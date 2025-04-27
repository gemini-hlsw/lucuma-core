// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.math.Angle
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS custom slit width.
 * @group Enumerations
 */
enum GmosCustomSlitWidth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val width: Angle
) derives Enumerated:

  case CustomWidth_0_25 extends GmosCustomSlitWidth("CustomWidth_0_25", "0.25\"", "0.25 arcsec", Angle.fromDoubleArcseconds(0.25))
  case CustomWidth_0_50 extends GmosCustomSlitWidth("CustomWidth_0_50", "0.50\"", "0.50 arcsec", Angle.fromDoubleArcseconds(0.50))
  case CustomWidth_0_75 extends GmosCustomSlitWidth("CustomWidth_0_75", "0.75\"", "0.75 arcsec", Angle.fromDoubleArcseconds(0.75))
  case CustomWidth_1_00 extends GmosCustomSlitWidth("CustomWidth_1_00", "1.00\"", "1.00 arcsec", Angle.fromDoubleArcseconds(1.00))
  case CustomWidth_1_50 extends GmosCustomSlitWidth("CustomWidth_1_50", "1.50\"", "1.50 arcsec", Angle.fromDoubleArcseconds(1.50))
  case CustomWidth_2_00 extends GmosCustomSlitWidth("CustomWidth_2_00", "2.00\"", "2.00 arcsec", Angle.fromDoubleArcseconds(2.00))
  case CustomWidth_5_00 extends GmosCustomSlitWidth("CustomWidth_5_00", "5.00\"", "5.00 arcsec", Angle.fromDoubleArcseconds(5.00))

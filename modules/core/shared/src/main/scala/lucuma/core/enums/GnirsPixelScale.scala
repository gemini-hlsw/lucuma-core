// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Pixel Scale.
 * @group Enumerations (Generated)
 */
enum GnirsPixelScale(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: BigDecimal
) derives Enumerated, Display:
    case PixelScale_0_05 extends GnirsPixelScale("PixelScale_0_05", "0.05 as/pix", "Pixel scale for short cameras", BigDecimal("0.05"))
    case PixelScale_0_15 extends GnirsPixelScale("PixelScale_0_15", "0.15 as/pix", "Pixel scale for long cameras", BigDecimal("0.15"))
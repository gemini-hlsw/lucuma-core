// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Camera.
 * @group Enumerations (Generated)
 */
enum GnirsCamera(
  val tag: String,
  val shortName: String,
  val longName: String,
  val pixelScale: GnirsPixelScale
) derives Enumerated, Display:
  case LongBlue extends GnirsCamera("LongBlue", "Long blue", "Long blue camera", GnirsPixelScale.PixelScale_0_05)
  case LongRed extends GnirsCamera("LongRed", "Long red", "Long red camera", GnirsPixelScale.PixelScale_0_05)
  case ShortBlue extends GnirsCamera("ShortBlue", "Short blue", "Short blue camera", GnirsPixelScale.PixelScale_0_15)
  case ShortRed extends GnirsCamera("ShortRed", "Short red", "Short red camera", GnirsPixelScale.PixelScale_0_15)
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
  case LongBlue extends GnirsCamera("LongBlue", "LB", "Long Blue", GnirsPixelScale.PixelScale_0_05)
  case LongRed extends GnirsCamera("LongRed", "LR", "Long Red", GnirsPixelScale.PixelScale_0_05)
  case ShortBlue extends GnirsCamera("ShortBlue", "SB", "Short Blue", GnirsPixelScale.PixelScale_0_15)
  case ShortRed extends GnirsCamera("ShortRed", "SR", "Short Red", GnirsPixelScale.PixelScale_0_15)

  // The blue and red cameras come in pairs sharing a pixel scale, and short and long are
  // never mixed within an observation, so changing colour always keeps the pixel scale.

  /** The blue camera with this camera's pixel scale; itself, when already blue. */
  def blue: GnirsCamera =
    pixelScale match
      case GnirsPixelScale.PixelScale_0_05 => GnirsCamera.LongBlue
      case GnirsPixelScale.PixelScale_0_15 => GnirsCamera.ShortBlue

  /** The red camera with this camera's pixel scale; itself, when already red. */
  def red: GnirsCamera =
    pixelScale match
      case GnirsPixelScale.PixelScale_0_05 => GnirsCamera.LongRed
      case GnirsPixelScale.PixelScale_0_15 => GnirsCamera.ShortRed
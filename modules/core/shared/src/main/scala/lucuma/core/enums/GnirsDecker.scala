// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNRIS Decker.
 * @group Enumerations (Generated)
 */
enum GnirsDecker(
  val tag: String,
  val shortName: String,
  val longName: String,
) derives Enumerated, Display:
  case Acquisition extends GnirsDecker("Acquisition", "Acquisition", "Acquisition")
  case PupilViewer extends GnirsDecker("PupilViewer", "Pupil", "Pupil viewer")
  case ShortCamCrossDispersed extends GnirsDecker("ShortCamCrossDispersed", "Short camera XD", "Short camera cross dispersed")
  case LongCamLongSlit extends GnirsDecker("LongCamLongSlit", "Long camera slit", "Long camera long slit")
  case ShortCamLongSlit extends GnirsDecker("ShortCamLongSlit", "Short camera slit", "Short camera long slit")
  case LongCamCrossDispersed extends GnirsDecker("LongCamCrossDispersed", "Long camera XD", "Long camera cross dispersed")

object GnirsDecker:
  // ATTENTION: This logic is duplicated in the DB view in the ODB. Modify it there too if it's changed here.
  def forCameraAndPrism(camera: GnirsCamera, prism: GnirsPrism): GnirsDecker =
    (prism, camera.pixelScale) match
      case (GnirsPrism.Mirror, GnirsPixelScale.PixelScale_0_15)               => GnirsDecker.ShortCamLongSlit
      case (GnirsPrism.Mirror, GnirsPixelScale.PixelScale_0_05)               => GnirsDecker.LongCamLongSlit
      case (GnirsPrism.Sxd | GnirsPrism.Lxd, GnirsPixelScale.PixelScale_0_15) => GnirsDecker.ShortCamCrossDispersed
      case (GnirsPrism.Sxd | GnirsPrism.Lxd, GnirsPixelScale.PixelScale_0_05) => GnirsDecker.LongCamCrossDispersed
    
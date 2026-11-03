// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  case Acquisition extends GnirsDecker("Acquisition", "Acq", "Acquisition")
  case PupilViewer extends GnirsDecker("PupilViewer", "Pupil", "Pupil viewer")
  case ShortCamCrossDispersed extends GnirsDecker("ShortCamCrossDispersed", "SC XD", "Short camera cross dispersed")
  case LongCamLongSlit extends GnirsDecker("LongCamLongSlit", "LC LS", "Long camera long slit")
  case ShortCamLongSlit extends GnirsDecker("ShortCamLongSlit", "SC LS", "Short camera long slit")
  case LongCamCrossDispersed extends GnirsDecker("LongCamCrossDispersed", "LC XD", "Long camera cross dispersed")
  case LowResolutionIfu extends GnirsDecker("LowResolutionIfu", "LR-IFU", "Low resolution IFU")
  case HighResolutionIfu extends GnirsDecker("HighResolutionIfu", "HR-IFU", "High resolution IFU")

object GnirsDecker:
  // ATTENTION: This logic is duplicated in the DB view in the ODB. Modify it there too if it's changed here.
  // The long-slit decker follows the camera; the cross-dispersed decker follows the prism, so the
  // long camera with the short cross-dispersing prism (LC SXD) uses the short camera XD decker.
  def forCameraAndPrism(camera: GnirsCamera, prism: GnirsPrism): GnirsDecker =
    (prism, camera.pixelScale) match
      case (GnirsPrism.Mirror, GnirsPixelScale.PixelScale_0_15) => GnirsDecker.ShortCamLongSlit
      case (GnirsPrism.Mirror, GnirsPixelScale.PixelScale_0_05) => GnirsDecker.LongCamLongSlit
      case (GnirsPrism.Sxd, _)                                  => GnirsDecker.ShortCamCrossDispersed
      case (GnirsPrism.Lxd, _)                                  => GnirsDecker.LongCamCrossDispersed

  // The IFU decker is determined solely by the IFU resolution (the long-slit
  // camera/prism derivation above does not apply to IFU configs).
  def forIfu(ifu: GnirsFpuIfu): GnirsDecker =
    ifu match
      case GnirsFpuIfu.LowResolution  => GnirsDecker.LowResolutionIfu
      case GnirsFpuIfu.HighResolution => GnirsDecker.HighResolutionIfu

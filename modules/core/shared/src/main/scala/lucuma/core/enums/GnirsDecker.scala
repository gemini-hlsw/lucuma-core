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
  val obsolete: Boolean
) derives Enumerated, Display:
  case Acquisition extends GnirsDecker("Acquisition", "Acquisition", "Acquisition", false)
  case PupilViewer extends GnirsDecker("PupilViewer", "Pupil", "Pupil viewer", false)
  case ShortCamCrossDispersed extends GnirsDecker("ShortCamCrossDispersed", "Short camera XD", "Short camera cross dispersed", false)
  case Ifu extends GnirsDecker("Ifu", "IFU", "Integral field unit", true)
  case LongCamLongSlit extends GnirsDecker("LongCamLongSlit", "Long camera slit", "Long camera long slit", false)
  case Wollaston extends GnirsDecker("Wollaston", "Wollaston", "Wollaston", true)
  case ShortCamLongSlit extends GnirsDecker("ShortCamLongSlit", "Short camera slit", "Short camera long slit", false)
  case LongCamCrossDispersed extends GnirsDecker("LongCamCrossDispersed", "Long camera XD", "Long camera cross dispersed", false)
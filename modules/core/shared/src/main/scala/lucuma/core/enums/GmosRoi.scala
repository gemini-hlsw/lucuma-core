// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS ROI (region of interest).
 * @group Enumerations (Generated)
 */
enum GmosRoi(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated, Display:

  case FullFrame       extends GmosRoi("FullFrame", "full", "Full Frame")
  case Ccd2            extends GmosRoi("Ccd2", "ccd2", "CCD 2")
  case CentralSpectrum extends GmosRoi("CentralSpectrum", "cspec", "Central Spectrum")
  case CentralStamp    extends GmosRoi("CentralStamp", "stamp", "Central Stamp")
  case Custom          extends GmosRoi("Custom", "custom", "Custom ROI")

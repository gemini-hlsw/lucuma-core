// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * GMOS IFU acquisition ROI options.
 *
 * Unlike the long slit these widen rather than narrow: the image through the IFU is always Full
 * Frame, because both bundles have to be visible and the sky bundle sits 60" from the target.
 *
 * @param tag database tag
 * @param name display name
 * @param imagingRoi ROI to use for the imaging step
 * @param ifuRoi ROI to use for the steps taken through the IFU
 */
enum GmosIfuAcquisitionRoi(
  val tag:        String,
  val name:       String,
  val imagingRoi: GmosRoi,
  val ifuRoi:     GmosRoi
) derives Enumerated, Display:

  case Ccd2FullFrame extends GmosIfuAcquisitionRoi(
    "Ccd2FullFrame",
    "CCD2 + Full Frame",
    GmosRoi.Ccd2,
    GmosRoi.FullFrame
  )

  case StampFullFrame extends GmosIfuAcquisitionRoi(
    "StampFullFrame",
    "Stamp + Full Frame",
    GmosRoi.CentralStamp,
    GmosRoi.FullFrame
  )

  case FullFrame extends GmosIfuAcquisitionRoi(
    "FullFrame",
    "Full Frame",
    GmosRoi.FullFrame,
    GmosRoi.FullFrame
  )

object GmosIfuAcquisitionRoi:
  /**
   * Selects the default acquisition ROI for science or calibration role.
   * calibrationRole = None => science
   */
  def defaultForScienceOrCalibration(calibrationRole: Option[CalibrationRole]): GmosIfuAcquisitionRoi =
    if calibrationRole.contains(CalibrationRole.SpectroPhotometric) then
      GmosIfuAcquisitionRoi.StampFullFrame
    else
      GmosIfuAcquisitionRoi.Ccd2FullFrame

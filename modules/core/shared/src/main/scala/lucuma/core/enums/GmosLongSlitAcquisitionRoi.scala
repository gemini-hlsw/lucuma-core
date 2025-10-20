// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * GMOS long slit acquisition ROI options.
 *
 * @param tag database tag
 * @param displayName display name
 * @param imagingRoi ROI to use for the imaging step
 * @param slitRoi ROI to use for the steps with a slit FPU option
 */
enum GmosLongSlitAcquisitionRoi(
  val tag:         String,
  val displayName: String,
  val imagingRoi:  GmosRoi,
  val slitRoi:     GmosRoi
) derives Enumerated:

  case Ccd2Stamp extends GmosLongSlitAcquisitionRoi(
    "Ccd2Stamp",
    "CCD2 + Stamp",
    GmosRoi.Ccd2,
    GmosRoi.CentralStamp
  )

  case Ccd2 extends GmosLongSlitAcquisitionRoi(
    "Ccd2",
    "CCD2",
    GmosRoi.Ccd2,
    GmosRoi.Ccd2
  )

  case Stamp extends GmosLongSlitAcquisitionRoi(
    "Stamp",
    "Stamp",
    GmosRoi.CentralStamp,
    GmosRoi.CentralStamp
  )

  case FullCcd2 extends GmosLongSlitAcquisitionRoi(
    "FullCcd2",
    "Full + CCD2",
    GmosRoi.FullFrame,
    GmosRoi.Ccd2
  )

object GmosLongSlitAcquisitionRoi:

  /**
   * Selects the default Acquisition ROI for the given science ROI and
   * calibration role.
   */
  def default(
    scienceRoi:      GmosRoi,
    calibrationRole: Option[CalibrationRole]
  ): GmosLongSlitAcquisitionRoi =

    if calibrationRole.contains(CalibrationRole.SpectroPhotometric) then
      GmosLongSlitAcquisitionRoi.Stamp
    else if scienceRoi === GmosRoi.CentralSpectrum then
      GmosLongSlitAcquisitionRoi.Ccd2Stamp
    else
      GmosLongSlitAcquisitionRoi.Ccd2
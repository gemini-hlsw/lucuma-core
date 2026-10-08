// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * The fixed value for the GHOST central wavelength.
 */
val CentralWavelength: Wavelength = Wavelength.fromIntNanometers(655).get

/**
 * Exposure time limits for each of the red and blue cameras.  Longer than
 * `MaxWarningExposureTime` is legal, but cosmic ray contamination becomes
 * excessive.
 */
val MinExposureTime: TimeSpan        = 100.msTimeSpan
val MaxExposureTime: TimeSpan        = 3600.secTimeSpan
val MaxWarningExposureTime: TimeSpan = 1800.secTimeSpan

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import lucuma.core.util.TimeSpan
import lucuma.core.syntax.timespan.*
import lucuma.core.math.Wavelength

val SetupTime: TimeSpan = 7.minTimeSpan
val DefaultExposureTime: TimeSpan = 30.secTimeSpan

val MinExposureTime: TimeSpan = 3008.msTimeSpan
val MaxExposureTime: TimeSpan = 600.secTimeSpan

val WavelengthCoverageLowerBound: Wavelength = Wavelength.fromIntNanometers(1490).get
val WavelengthCoverageUpperBound: Wavelength = Wavelength.fromIntNanometers(2460).get

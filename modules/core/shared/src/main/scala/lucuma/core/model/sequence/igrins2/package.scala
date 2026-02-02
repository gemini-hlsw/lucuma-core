// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import lucuma.core.enums.Igrins2FowlerSamples
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.core.util.NewBoolean
import lucuma.core.util.TimeSpan

object Igrins2SvcImages extends NewBoolean:
  val Save = True
  val DontSave = False

type Igrins2SvcImages = Igrins2SvcImages.Type

val SetupTime: TimeSpan = 7.minTimeSpan
val ReAcquisitionTime: TimeSpan = 5.minTimeSpan
val DefaultExposureTime: TimeSpan = 30.secTimeSpan

val MinExposureTime: TimeSpan = 3080.msTimeSpan
val MaxExposureTime: TimeSpan = 600.secTimeSpan

val WavelengthCoverageLowerBound: Wavelength = Wavelength.fromIntNanometers(1490).get
val WavelengthCoverageUpperBound: Wavelength = Wavelength.fromIntNanometers(2460).get

def fowlerSamplesForExposureTime(exposure: TimeSpan): Igrins2FowlerSamples =
  val seconds = exposure.toSeconds.toDouble
  val nFowler = ((seconds - 1.45479 - 0.168) / 1.45479).toInt
  Igrins2FowlerSamples.values.reverse
    .find(fs => nFowler >= (1 << fs.ordinal))
    .getOrElse(Igrins2FowlerSamples.One)

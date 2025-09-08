// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.enums.ScienceMode.*
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbTimeSpan
import monocle.law.discipline.LensTests
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite

class ExposureTimeModeSuite extends DisciplineSuite:
  import ArbExposureTimeMode.given
  import ArbRefined.given
  import ArbSignalToNoise.given
  import ArbTimeSpan.given
  import ArbWavelength.given
  import ExposureTimeMode.*
  import ExposureTimeMode.SignalToNoiseMode.given

  checkAll("Eq[ExposureTimeMode[Spectroscopy]]", EqTests[ExposureTimeMode[Spectroscopy.type]].eqv)
  checkAll("Eq[ExposureTimeMode[Imaging]]", EqTests[ExposureTimeMode[Imaging          .type]].eqv)

  checkAll("ExposureTimeMode.signalToNoise[Spectroscopy]", PrismTests(signalToNoise[Spectroscopy.type]))
  checkAll("ExposureTimeMode.signalToNoise[Imaging]",      PrismTests(signalToNoise[Imaging.type]))
  checkAll("ExposureTimeMode.timeAndCount[Spectroscopy]",  PrismTests(timeAndCount[Imaging.type]))
  checkAll("ExposureTimeMode.timeAndCount[Imaging]",       PrismTests(timeAndCount[Imaging.type]))
  checkAll("ExposureTimeMode.at[Spectroscopy]",            LensTests(at[Spectroscopy.type]))
  checkAll("ExposureTimeMode.at[Imaging]",                 LensTests(at[Imaging.type]))

  checkAll("Eq[SignalToNoiseMode[Spectroscopy]]",   EqTests[SignalToNoiseMode[Spectroscopy.type]].eqv)
  checkAll("Eq[SignalToNoiseMode[Imaging]]",        EqTests[SignalToNoiseMode[Imaging.type]].eqv)
  checkAll("SignalToNoiseMode.value[Spectroscopy]", LensTests(SignalToNoiseMode.value[Spectroscopy.type]))
  checkAll("SignalToNoiseMode.value[Imaging]",      LensTests(SignalToNoiseMode.value[Imaging.type]))
  checkAll("SignalToNoiseMode.at[Spectroscopy]",    LensTests(SignalToNoiseMode.at[Spectroscopy.type]))
  checkAll("SignalToNoiseMode.at[Imaging]",         LensTests(SignalToNoiseMode.at[Imaging.type]))

  checkAll("Eq[TimeAndCountMode[Spectroscopy]]",   EqTests[TimeAndCountMode[Spectroscopy.type]].eqv)
  checkAll("Eq[TimeAndCountMode[Imaging]]",        EqTests[TimeAndCountMode[Imaging.type]].eqv)
  checkAll("TimeAndCountMode.time[Spectroscopy]",  LensTests(TimeAndCountMode.time[Spectroscopy.type]))
  checkAll("TimeAndCountMode.time[Imaging]",       LensTests(TimeAndCountMode.time[Imaging.type]))
  checkAll("TimeAndCountMode.count[Spectroscopy]", LensTests(TimeAndCountMode.count[Spectroscopy.type]))
  checkAll("TimeAndCountMode.count[Imaging]",      LensTests(TimeAndCountMode.count[Imaging.type]))
  checkAll("TimeAndCountMode.at[Spectroscopy]",    LensTests(TimeAndCountMode.at[Spectroscopy.type]))
  checkAll("TimeAndCountMode.at[Imaging]",         LensTests(TimeAndCountMode.at[Imaging.type]))

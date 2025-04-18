// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
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

  checkAll("Eq[ExposureTimeMode]", EqTests[ExposureTimeMode].eqv)
  checkAll("ExposureTimeMode.signalToNoise", PrismTests(signalToNoise))
  checkAll("ExposureTimeMode.timeAndCount",  PrismTests(timeAndCount))
  checkAll("ExposureTimeMode.at",            LensTests(at))

  checkAll("Eq[SignalToNoiseMode]",   EqTests[SignalToNoiseMode].eqv)
  checkAll("SignalToNoiseMode.value", LensTests(SignalToNoiseMode.value))
  checkAll("SignalToNoiseMode.at",    LensTests(SignalToNoiseMode.at))

  checkAll("Eq[TimeAndCountMode]",   EqTests[TimeAndCountMode].eqv)
  checkAll("TimeAndCountMode.time",  LensTests(TimeAndCountMode.time))
  checkAll("TimeAndCountMode.count", LensTests(TimeAndCountMode.count))
  checkAll("TimeAndCountMode.at",    LensTests(TimeAndCountMode.at))
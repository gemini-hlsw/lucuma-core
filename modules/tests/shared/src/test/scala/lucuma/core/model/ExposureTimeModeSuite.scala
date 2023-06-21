// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.util.arb.ArbTimeSpan
import monocle.law.discipline.LensTests
import monocle.law.discipline.OptionalTests
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite

class ExposureTimeModeSuite extends DisciplineSuite {
  import ArbExposureTimeMode.given
  import ArbRefined.given
  import ArbSignalToNoise.given
  import ArbTimeSpan.given

  checkAll("Eq[ExposureTimeMode]", EqTests[ExposureTimeMode].eqv)
  checkAll("ExposureTimeMode.signalToNoise", PrismTests(ExposureTimeMode.signalToNoise))
  checkAll("ExposureTimeMode.fixedExposure", PrismTests(ExposureTimeMode.fixedExposure))
  checkAll("ExposureTimeMode.signalToNoiseValue",
           OptionalTests(ExposureTimeMode.signalToNoiseValue)
  )
  checkAll("ExposureTimeMode.exposureCount", OptionalTests(ExposureTimeMode.exposureCount))
  checkAll("ExposureTimeMode.exposureTime", OptionalTests(ExposureTimeMode.exposureTime))

  checkAll("Eq[SignalToNoise]", EqTests[ExposureTimeMode.SignalToNoiseMode].eqv)
  checkAll("SignalToNoise.value", LensTests(ExposureTimeMode.SignalToNoiseMode.value))

  checkAll("Eq[FixedExposure]", EqTests[ExposureTimeMode.FixedExposureMode].eqv)
  checkAll("FixedExposure.count", LensTests(ExposureTimeMode.FixedExposureMode.count))
  checkAll("FixedExposure.time", LensTests(ExposureTimeMode.FixedExposureMode.time))
}

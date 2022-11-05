// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.arb.ArbExposureTimeMode
import lucuma.core.model.arb.ArbNonNegDuration
import lucuma.core.model.given
import monocle.law.discipline.LensTests
import monocle.law.discipline.OptionalTests
import monocle.law.discipline.PrismTests
import munit.DisciplineSuite

class ExposureTimeModeSuite extends DisciplineSuite {
  import ArbNonNegDuration.given
  import ArbExposureTimeMode.given
  import ArbRefined.*

  checkAll("Eq[ExposureTimeMode]", EqTests[ExposureTimeMode].eqv)
  checkAll("ExposureTimeMode.signalToNoise", PrismTests(ExposureTimeMode.signalToNoise))
  checkAll("ExposureTimeMode.fixedExposure", PrismTests(ExposureTimeMode.fixedExposure))
  checkAll("ExposureTimeMode.signalToNoiseValue",
           OptionalTests(ExposureTimeMode.signalToNoiseValue)
  )
  checkAll("ExposureTimeMode.exposureCount", OptionalTests(ExposureTimeMode.exposureCount))
  checkAll("ExposureTimeMode.exposureTime", OptionalTests(ExposureTimeMode.exposureTime))

  checkAll("Eq[SignalToNoise]", EqTests[ExposureTimeMode.SignalToNoise].eqv)
  checkAll("SignalToNoise.value", LensTests(ExposureTimeMode.SignalToNoise.value))

  checkAll("Eq[FixedExposure]", EqTests[ExposureTimeMode.FixedExposure].eqv)
  checkAll("FixedExposure.count", LensTests(ExposureTimeMode.FixedExposure.count))
  checkAll("FixedExposure.time", LensTests(ExposureTimeMode.FixedExposure.time))
}

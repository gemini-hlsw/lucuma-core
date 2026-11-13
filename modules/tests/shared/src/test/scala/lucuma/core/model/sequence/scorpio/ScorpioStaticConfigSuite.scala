// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioStaticConfig.given
import monocle.law.discipline.LensTests
import munit.*

class ScorpioStaticConfigSuite extends DisciplineSuite:

  import lucuma.core.model.sequence.scorpio.arb.ArbScorpioNodAndShift.given
  import lucuma.core.util.arb.ArbEnumerated.given
  import lucuma.core.util.arb.ArbTimeSpan.given

  checkAll("Eq[ScorpioStaticConfig]", EqTests[ScorpioStaticConfig].eqv)
  checkAll("ScorpioStaticConfig.channelSet", LensTests(ScorpioStaticConfig.channelSet))
  checkAll("ScorpioStaticConfig.adc", LensTests(ScorpioStaticConfig.adc))
  checkAll("ScorpioStaticConfig.svcExposureTime", LensTests(ScorpioStaticConfig.svcExposureTime))
  checkAll("ScorpioStaticConfig.nodAndShift", LensTests(ScorpioStaticConfig.nodAndShift))

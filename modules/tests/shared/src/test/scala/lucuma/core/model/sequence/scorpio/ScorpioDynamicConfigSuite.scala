// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.kernel.laws.discipline.*
import cats.syntax.order.*
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.enums.ScorpioFpu
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioDetector.given
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioDynamicConfig.given
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioFpuMask.given
import lucuma.core.util.TimeSpan
import lucuma.refined.*
import munit.*
import org.scalacheck.Prop.forAll

class ScorpioDynamicConfigSuite extends DisciplineSuite:

  checkAll("Eq[ScorpioDetector]", EqTests[ScorpioDetector].eqv)
  checkAll("Eq[ScorpioFpuMask]", EqTests[ScorpioFpuMask].eqv)
  checkAll("Eq[ScorpioDynamicConfig]", EqTests[ScorpioDynamicConfig].eqv)

  test("totalExposureTime is the longest active detector"):
    forAll: (c: ScorpioDynamicConfig) =>
      val totals = c.detectors.map(_.totalExposureTime)
      assert(totals.forall(_ <= c.totalExposureTime))
      assert(totals.isEmpty || totals.contains(c.totalExposureTime))

  test("totalExposureTime is zero when all detectors are off"):
    val off = ScorpioDynamicConfig(None, None, None, None, None, None, None, None, ScorpioFpuMask.Imaging, ScorpioDisperser.Out)
    assertEquals(off.totalExposureTime, TimeSpan.Zero)

  test("totalExposureTime multiplies time by count"):
    val d   = ScorpioDetector(TimeSpan.unsafeFromMicroseconds(10_000_000L), 3.refined)
    val cfg = ScorpioDynamicConfig(Some(d), None, None, None, None, None, None, None, ScorpioFpuMask.LongSlit(ScorpioFpu.LongSlit_0_54), ScorpioDisperser.Vph)
    assertEquals(cfg.totalExposureTime, TimeSpan.unsafeFromMicroseconds(30_000_000L))

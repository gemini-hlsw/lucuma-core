// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.kernel.laws.discipline.*
import cats.syntax.order.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ScorpioDisperser
import lucuma.core.enums.ScorpioFpu
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioDetector.given
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioDynamicConfig.given
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioFpuMask.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbTimeSpan.given
import monocle.law.discipline.LensTests
import monocle.law.discipline.PrismTests
import munit.*
import org.scalacheck.Prop.forAll

class ScorpioDynamicConfigSuite extends DisciplineSuite:

  checkAll("Eq[ScorpioDetector]", EqTests[ScorpioDetector].eqv)
  checkAll("ScorpioDetector.exposureTime", LensTests(ScorpioDetector.exposureTime))
  checkAll("ScorpioDetector.exposureCount", LensTests(ScorpioDetector.exposureCount))

  checkAll("Eq[ScorpioFpuMask]", EqTests[ScorpioFpuMask].eqv)
  checkAll("ScorpioFpuMask.longSlit", PrismTests(ScorpioFpuMask.longSlit))

  checkAll("Eq[ScorpioDynamicConfig]", EqTests[ScorpioDynamicConfig].eqv)
  checkAll("ScorpioDynamicConfig.g", LensTests(ScorpioDynamicConfig.g))
  checkAll("ScorpioDynamicConfig.ks", LensTests(ScorpioDynamicConfig.ks))
  checkAll("ScorpioDynamicConfig.fpu", LensTests(ScorpioDynamicConfig.fpu))
  checkAll("ScorpioDynamicConfig.disperser", LensTests(ScorpioDynamicConfig.disperser))

  test("totalExposureTime is the longest active detector"):
    forAll: (c: ScorpioDynamicConfig) =>
      val totals = c.detectors.map(_.totalExposureTime)
      assert(totals.forall(_ <= c.totalExposureTime))
      assert(totals.isEmpty || totals.contains(c.totalExposureTime))

  test("totalExposureTime is zero when all detectors are off"):
    val off = ScorpioDynamicConfig(None, None, None, None, None, None, None, None, ScorpioFpuMask.Imaging, ScorpioDisperser.Out)
    assertEquals(off.totalExposureTime, TimeSpan.Zero)

  test("totalExposureTime multiplies time by count"):
    val d   = ScorpioDetector(TimeSpan.unsafeFromMicroseconds(10_000_000L), PosInt.unsafeFrom(3))
    val cfg = ScorpioDynamicConfig(Some(d), None, None, None, None, None, None, None, ScorpioFpuMask.LongSlit(ScorpioFpu.LongSlit_0_54), ScorpioDisperser.Vph)
    assertEquals(cfg.totalExposureTime, TimeSpan.unsafeFromMicroseconds(30_000_000L))

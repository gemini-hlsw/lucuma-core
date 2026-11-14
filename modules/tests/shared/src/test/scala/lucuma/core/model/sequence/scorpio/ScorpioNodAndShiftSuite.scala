// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.model.sequence.scorpio.arb.ArbScorpioNodAndShift.given
import monocle.law.discipline.LensTests
import munit.*

class ScorpioNodAndShiftSuite extends DisciplineSuite:

  checkAll("Eq[ScorpioNodAndShift]", EqTests[ScorpioNodAndShift].eqv)
  checkAll("ScorpioNodAndShift.posA", LensTests(ScorpioNodAndShift.posA))
  checkAll("ScorpioNodAndShift.posB", LensTests(ScorpioNodAndShift.posB))
  checkAll("ScorpioNodAndShift.shiftOffset", LensTests(ScorpioNodAndShift.shiftOffset))
  checkAll("ScorpioNodAndShift.shiftCycles", LensTests(ScorpioNodAndShift.shiftCycles))

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.truncated

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.truncated.arb.ArbTruncatedAngle._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class TruncatedAngleSpec extends DisciplineSuite {
  import ArbAngle._

  // Laws
  checkAll("Eq[TruncatedAngle]", EqTests[TruncatedAngle].eqv)

  // Optics
  checkAll("TruncatedAngle.angle", SplitEpiTests(TruncatedAngle.angle).splitEpi)
}

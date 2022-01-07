// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
import cats.kernel.laws.discipline._
import lucuma.core.math.arb.ArbProperMotion._
import lucuma.core.optics.laws.discipline.SplitMonoTests
import munit.DisciplineSuite

final class ProperMotionSuite extends DisciplineSuite {

  // Laws
  checkAll("Order[ProperMotion]", OrderTests[ProperMotion].order)
  checkAll("Monoid[ProperMotion]", MonoidTests[ProperMotion].monoid)

  checkAll("milliarcsecondsPerYear", SplitMonoTests(ProperMotion.milliarcsecondsPerYear).splitMono)

}

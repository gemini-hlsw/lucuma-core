// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline._
import lucuma.core.math.arb.ArbProperMotion._
import lucuma.core.model.SiderealTracking
import lucuma.core.optics.laws.discipline.SplitMonoTests
import lucuma.core.syntax.instant.*
import lucuma.core.util.Timestamp
import munit.DisciplineSuite

import java.time.Instant

final class ProperMotionSuite extends DisciplineSuite {

  // Laws
  checkAll("Order[ProperMotion]", OrderTests[ProperMotion].order)
  checkAll("Monoid[ProperMotion]", MonoidTests[ProperMotion].monoid)

  checkAll("milliarcsecondsPerYear", SplitMonoTests(ProperMotion.milliarcsecondsPerYear).splitMono)

  test("Sanity checks") {
    val tracking = SiderealTracking(
        Coordinates.Zero,
        Epoch.J2000,
        Some(ProperMotion.milliarcsecondsPerYear.reverseGet((-3000, 4000))),
        RadialVelocity.kilometerspersecond.getOption(10000),
        Some(Parallax.fromMicroarcseconds(10000000L))
      )

    // July 4th 2022, around mid day
    val instant = Instant.ofEpochMilli(1656966489).toTimestamp
    assertEquals(instant.flatMap(tracking.at), Some(Coordinates.fromHmsDms.getOption("11:59:57.096334 -00:00:58.073307").get))
  }
}

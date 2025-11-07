// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline.*
import lucuma.core.math.arb.ArbProperMotion.given
import lucuma.core.model.SiderealTracking
import lucuma.core.optics.laws.discipline.SplitMonoTests
import munit.DisciplineSuite

import java.time.Instant

final class ProperMotionSuite extends DisciplineSuite {

  // Laws
  checkAll("Order[ProperMotion]", OrderTests[ProperMotion].order)
  checkAll("Monoid[ProperMotion]", MonoidTests[ProperMotion].monoid)
  checkAll("Order[AngularVelocityComponent]", OrderTests[ProperMotion.RA].order)
  checkAll("Monoid[AngularVelocityComponent]", MonoidTests[ProperMotion.RA].monoid)
  checkAll("Order[AngularVelocityComponent]", OrderTests[ProperMotion.Dec].order)
  checkAll("Monoid[AngularVelocityComponent]", MonoidTests[ProperMotion.Dec].monoid)

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
    val instant = Instant.ofEpochMilli(1656966489)
    assertEquals(tracking.at(instant), Some(Coordinates.fromHmsDms.getOption("11:59:57.096352 -00:00:58.072942").get))
  }
}

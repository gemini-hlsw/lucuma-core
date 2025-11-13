// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline.*
import lucuma.core.math.arb.ArbProperMotion.given
import lucuma.core.model.SiderealTracking
import lucuma.core.optics.laws.discipline.SplitMonoTests
import munit.DisciplineSuite

import java.time.LocalDateTime

final class ProperMotionSuite extends DisciplineSuite {

  // Laws
  checkAll("Order[ProperMotion]", OrderTests[ProperMotion].order)
  checkAll("Monoid[ProperMotion]", MonoidTests[ProperMotion].monoid)
  checkAll("Order[AngularVelocityComponent]", OrderTests[ProperMotion.RA].order)
  checkAll("Monoid[AngularVelocityComponent]", MonoidTests[ProperMotion.RA].monoid)
  checkAll("Order[AngularVelocityComponent]", OrderTests[ProperMotion.Dec].order)
  checkAll("Monoid[AngularVelocityComponent]", MonoidTests[ProperMotion.Dec].monoid)

  checkAll("milliarcsecondsPerYear", SplitMonoTests(ProperMotion.milliarcsecondsPerYear).splitMono)

  test("Sanity checks") { // checked with astropy
    val tracking = SiderealTracking(
        Coordinates(RightAscension.fromDoubleDegrees(0.0), Declination.fromDoubleDegrees(80.0).get),
        Epoch.J2000,
        Some(ProperMotion.milliarcsecondsPerYear.reverseGet((-3000, 4000))),
        RadialVelocity.kilometerspersecond.getOption(50),
        Some(Parallax.fromMicroarcseconds(10000000L))
      )

    // July 4th 2022, noon UTC
    val instant = LocalDateTime.of(2022, 7, 4, 12, 0).toInstant(java.time.ZoneOffset.UTC)
    assertEquals(tracking.at(instant), Some(Coordinates.fromHmsDms.getOption("23:59:34.311844 +80:01:28.934422").get))
  }
}

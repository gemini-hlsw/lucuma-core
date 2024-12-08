// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import lucuma.core.model.arb.*
import munit.DisciplineSuite

class SiteCoordinatesLimitsSuite extends DisciplineSuite {
  import ArbSiteCoordinatesLimits.given

  checkAll("Eq[SiteCoordinatesLimits.Eq]", EqTests[SiteCoordinatesLimits].eqv)

  test("check dec limits") {
    val sc = SiteCoordinatesLimits(
      RightAscension(HourAngle.fromDoubleHours(16.5)),
      RightAscension(HourAngle.fromDoubleHours(14.0)),
      Declination.fromDoubleDegrees(-37.0).get,
      Declination.fromDoubleDegrees(90.0).get
    )

    assert(sc.inDecLimits(Declination.fromDoubleDegrees(0).get))
    assert(sc.inDecLimits(Declination.fromDoubleDegrees(-37).get))
    assert(!sc.inDecLimits(Declination.fromDoubleDegrees(-38).get))
    assert(sc.inDecLimits(Declination.fromDoubleDegrees(89).get))
  }

  test("check ra limits") {
    val sc = SiteCoordinatesLimits(
      RightAscension(HourAngle.fromDoubleHours(14.0)),
      RightAscension(HourAngle.fromDoubleHours(15.0)),
      Declination.fromDoubleDegrees(-37.0).get,
      Declination.fromDoubleDegrees(90.0).get
    )

    assert(!sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(15.1))))
    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(15.0))))
    assert(!sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(13.9))))
    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(14.0))))
    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(14.5))))
    assert(!sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(0.0))))
  }

  test("check ra limits with wrap") {
    val sc = SiteCoordinatesLimits(
      RightAscension(HourAngle.fromDoubleHours(16.5)),
      RightAscension(HourAngle.fromDoubleHours(14.0)),
      Declination.fromDoubleDegrees(-37.0).get,
      Declination.fromDoubleDegrees(90.0).get
    )

    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(16.5))))
    assert(!sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(16.4))))
    assert(!sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(15.0))))
    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(13.0))))
    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(14.0))))
    assert(!sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(14.1))))
    assert(sc.inRaLimits(RightAscension(HourAngle.fromDoubleHours(1.0))))
  }

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import lucuma.core.enum.Site
import lucuma.core.enum.TwilightType
import munit.FunSuite

import java.time.LocalDate

final class TwilightCalcSuite extends FunSuite {
  private val Date = LocalDate.of(2000, 1, 1)

  // Known results with OCS
  private val expected: Map[(Site, TwilightType, LocalDate), (Long, Long)] =
    Map(
      (Site.GN, TwilightType.Official, Date)     -> ((946785833352L, 946831636357L)),
      (Site.GN, TwilightType.Civil, Date)        -> ((946786687500L, 946830782669L)),
      (Site.GN, TwilightType.Nautical, Date)     -> ((946788331171L, 946829139780L)),
      (Site.GN, TwilightType.Astronomical, Date) -> ((946789955319L, 946827516290L)),
      (Site.GS, TwilightType.Official, Date)     -> ((946771013083L, 946805782320L)),
      (Site.GS, TwilightType.Civil, Date)        -> ((946772131479L, 946804663970L)),
      (Site.GS, TwilightType.Nautical, Date)     -> ((946774129507L, 946802666017L)),
      (Site.GS, TwilightType.Astronomical, Date) -> ((946776266779L, 946800528816L))
    )

  test("TwilightCalcSpec: Sunrise and sunset on 2000-01-01") {
    expected.foreach { case ((site, tbType, date), (s, e)) =>
      val interval = TwilightCalc.forDate(tbType, date, site.place).get
      // The use of a different JulianDate throughout the calculations produces a very slight difference,
      // therefore we allow a couple of milliseconds of tolerance.
      assertEqualsDouble(interval.lower.toEpochMilli.toDouble, s.toDouble, 2)
      assertEqualsDouble(interval.upper.toEpochMilli.toDouble, e.toDouble, 2)
    }
  }
}

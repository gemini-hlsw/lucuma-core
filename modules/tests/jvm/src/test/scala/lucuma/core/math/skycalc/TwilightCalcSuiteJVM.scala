// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

import cats._
import cats.implicits._
import lucuma.core.math.skycalc.TwilightCalc
import java.time.Instant
import java.time.LocalDate
import lucuma.core.enum.Site
import lucuma.core.enum.TwilightType
import lucuma.core.math.Interval
import lucuma.core.math.arb.ArbTime._
import lucuma.core.util.arb.ArbEnumerated._
import edu.gemini.skycalc.TwilightBoundedNightTest
import org.scalactic.Tolerance

final class TwilightCalcSuiteJVM extends ScalaCheckSuite with Tolerance {

  implicit val showLocalDate: Show[LocalDate] = Show.fromToString
  implicit val InstantEq: Eq[Instant]         = Eq.fromUniversalEquals

  test("TwilightCalcSpec: Arbitrary sky calculations") {
    // TwilightCalc returns sunrise before sunset in some cases if we use arbitrary Place.
    // Therefore, we only test for our Sites, where it works as expected.
    forAll { (twilightType: TwilightType, localDate: LocalDate, site: Site) =>
      val (start, end) = TwilightCalc
        .forDate(twilightType, localDate, site.place)
        .map(Interval.fromInstants.reverseGet.andThen(_.bimap(_.toEpochMilli, _.toEpochMilli)))
        .getOrElse((0L, 0L))
      val tbn          =
        TwilightBoundedNightTest.forDate(TwilightTypeJVM(twilightType),
                                         localDate.getDayOfMonth,
                                         localDate.getMonthValue - 1,
                                         localDate.getYear,
                                         site.place
        )

      // The use of a different JulianDate implementation throughout the calculations produces
      // a very slight difference, therefore we allow a couple of milliseconds of tolerance.
      assert((start +- 2).isWithin(tbn.getStartTime))
      assert((end +- 2).isWithin(tbn.getEndTime))
    }
  }
}

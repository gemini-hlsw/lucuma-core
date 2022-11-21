// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats._
import cats.syntax.all._
import edu.gemini.skycalc.TwilightBoundedNightTest
import lucuma.core.arb.ArbTime
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.skycalc.TwilightCalc
import lucuma.core.optics.Spire
import lucuma.core.util.arb.ArbEnumerated
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.typelevel.cats.time._

import java.time.Instant
import java.time.LocalDate

final class TwilightCalcSuiteJVM extends ScalaCheckSuite {
  import ArbEnumerated._
  import ArbTime._

  implicit val showLocalDate: Show[LocalDate] = Show.fromToString
  implicit val InstantEq: Eq[Instant]         = Eq.fromUniversalEquals

  test("TwilightCalcSpec: Arbitrary sky calculations") {
    // TwilightCalc returns sunrise before sunset in some cases if we use arbitrary Place.
    // Therefore, we only test for our Sites, where it works as expected.
    forAll { (twilightType: TwilightType, localDate: LocalDate, site: Site) =>
      val (start, end) = TwilightCalc
        .forDate(twilightType, localDate, site.place)
        .map(Spire.openUpperIntervalFromTuple[Instant].reverseGet.andThen(_.bimap(_.toEpochMilli, _.toEpochMilli)))
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
      assertEqualsDouble(start.toDouble, tbn.getStartTime.toDouble, 2)
      assertEqualsDouble(end.toDouble, tbn.getEndTime.toDouble, 2)
      assertEqualsDouble(end.toDouble, tbn.getEndTime.toDouble, 2)
      assertEqualsDouble(end.toDouble, tbn.getEndTime.toDouble, 2)
    }
  }
}

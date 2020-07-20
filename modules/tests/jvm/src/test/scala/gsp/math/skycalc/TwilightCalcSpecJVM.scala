// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import weaver._
import weaver.scalacheck._

import cats._
import cats.implicits._
import gsp.math.skycalc.TwilightCalc
import java.time.LocalDate
import gsp.math.Place
import gsp.math.arb.ArbTime._
import gsp.math.arb.ArbPlace._
import gsp.math.arb.ArbTwilightBoundType._
import edu.gemini.skycalc.TwilightBoundedNightTest
import java.time.Instant
import org.scalactic.Tolerance

object TwilightCalcSpecJVM extends SimpleIOSuite with IOCheckers with Tolerance {

  implicit val showLocalDate: Show[LocalDate] = Show.fromToString
  implicit val InstantEq: Eq[Instant]         = Eq.fromUniversalEquals

  simpleTest("TwilightCalcSpec: Arbitrary sky calculations") {
    forall { (boundType: TwilightBoundType, localDate: LocalDate, place: Place) =>
      val (start, end) = TwilightCalc
        .calculate(boundType, localDate, place)
        .map(_.bimap(_.toEpochMilli, _.toEpochMilli))
        .getOrElse((0L, 0L))
      val tbn          =
        TwilightBoundedNightTest.forDate(boundType,
                                         localDate.getDayOfMonth,
                                         localDate.getMonthValue - 1,
                                         localDate.getYear,
                                         place
        )

      // The use of a different JulianDate throughout the calculations produces a very slight difference,
      // therefore we allow a couple of milliseconds of tolerance.
      assert((start +- 2).isWithin(tbn.getStartTime))
        .and(assert((end +- 2).isWithin(tbn.getEndTime)))
    }
  }
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import cats.implicits._
import weaver._
import weaver.scalacheck._

import edu.gemini.skycalc.ImprovedSkyCalcTest
import cats.Show
import java.time.Instant
import gsp.math.Coordinates
import gsp.math.arb.ArbAngle._
import gsp.math.arb.ArbCoordinates._
import com.fortysevendeg.scalacheck.datetime.instances.jdk8._
import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import java.time._
import gsp.math.Angle

object ImprovedSkyCalcSpec extends SimpleIOSuite with IOCheckers {

  implicit val showInstant: Show[Instant]   = Show.fromToString
  implicit val showZDT: Show[ZonedDateTime] = Show.fromToString

  private val zdtFrom  = ZonedDateTime.of(
    LocalDate.of(1901, 1, 1),
    LocalTime.MIDNIGHT,
    ZoneOffset.UTC
  )
  private val zdtRange = Duration.ofDays(Period.ofYears(1000).getDays.toLong)

  simpleTest("ImprovedSkyCalcSpec: Arbitrary sky calculations") {
    forall { (latitude: Angle, longitude: Angle, altitude: Int) =>
      // This generator already provides ZDTs with millisecond precision, not nano.
      forall(genDateTimeWithinRange(zdtFrom, zdtRange)) { zdt =>
        forall { coords: Coordinates =>
          val calc     = new ImprovedSkyCalc(latitude, longitude, altitude)
          val javaCalc = new ImprovedSkyCalcTest(latitude, longitude, altitude)

          val instant = zdt.toInstant

          calc.calculate(coords, instant, false)
          javaCalc.calculate(coords, instant, false)

          expect(calc.getAltitude == javaCalc.getAltitude)
          expect(calc.getAzimuth == javaCalc.getAzimuth)
          expect(calc.getParallacticAngle == javaCalc.getParallacticAngle)
          expect(calc.getHourAngle == javaCalc.getHourAngle)
          expect(calc.getLunarIlluminatedFraction == javaCalc.getLunarIlluminatedFraction)
          expect(calc.getLunarSkyBrightness == javaCalc.getLunarSkyBrightness)
          expect(calc.getTotalSkyBrightness == javaCalc.getTotalSkyBrightness)
          expect(calc.getLunarPhaseAngle == javaCalc.getLunarPhaseAngle)
          expect(calc.getSunAltitude == javaCalc.getSunAltitude)
          expect(calc.getLunarDistance == javaCalc.getLunarDistance)
          expect(calc.getLunarElevation == javaCalc.getLunarElevation)
        }
      }
    }
  }
}

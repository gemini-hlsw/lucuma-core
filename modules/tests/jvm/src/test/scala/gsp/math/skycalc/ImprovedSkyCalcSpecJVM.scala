// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats.implicits._
import weaver._
import weaver.scalacheck._

import edu.gemini.skycalc.ImprovedSkyCalcTest
import cats.Show
import java.time.Instant
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates._
import lucuma.core.math.arb.ArbPlace._
import com.fortysevendeg.scalacheck.datetime.instances.jdk8._
import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import java.time._
import lucuma.core.math.Place
import jsky.coords.WorldCoords
import java.{ util => ju }

object ImprovedSkyCalcSpecJVM extends SimpleIOSuite with IOCheckers {

  implicit val showInstant: Show[Instant]   = Show.fromToString
  implicit val showZDT: Show[ZonedDateTime] = Show.fromToString

  private val zdtFrom  = ZonedDateTime.of(
    LocalDate.of(1901, 1, 1),
    LocalTime.MIDNIGHT,
    ZoneOffset.UTC
  )
  private val zdtRange = Duration.ofDays(Period.ofYears(1000).getDays.toLong)

  simpleTest("ImprovedSkyCalcSpec: Arbitrary sky calculations") {
    forall { place: Place =>
      // This SkyCalc should be thread safe, but Java's isn't.
      val calc = ImprovedSkyCalc(place)

      // This generator already provides ZDTs with millisecond precision, not nano.
      forall(genDateTimeWithinRange(zdtFrom, zdtRange)) { zdt =>
        val instant = zdt.toInstant

        forall { coords: Coordinates =>
          val javaCalc = new ImprovedSkyCalcTest(place.latitude.toAngle.toSignedDoubleDegrees,
                                                 place.longitude.toSignedDoubleDegrees,
                                                 place.altitudeDouble
          )

          val results = calc.calculate(coords, instant, false)
          javaCalc.calculate(new WorldCoords(coords.ra.toAngle.toSignedDoubleDegrees,
                                             coords.dec.toAngle.toSignedDoubleDegrees
                             ),
                             ju.Date.from(instant),
                             false
          )

          expect(results.altitudeRaw === javaCalc.getAltitude)
            .and(expect(results.azimuthRaw === javaCalc.getAzimuth))
            .and(expect(results.parallacticAngleRaw === javaCalc.getParallacticAngle))
            .and(expect(results.hourAngleRaw === javaCalc.getHourAngle))
            .and(expect(results.lunarIlluminatedFraction === javaCalc.getLunarIlluminatedFraction))
            .and(expect(results.lunarSkyBrightness === javaCalc.getLunarSkyBrightness))
            .and(expect(results.totalSkyBrightness === javaCalc.getTotalSkyBrightness))
            .and(expect(results.lunarPhaseAngleRaw === javaCalc.getLunarPhaseAngle))
            .and(expect(results.sunAltitudeRaw === javaCalc.getSunAltitude))
            .and(expect(results.lunarDistance === javaCalc.getLunarDistance))
            .and(expect(results.lunarElevationRaw === javaCalc.getLunarElevation))
        }
      }
    }
  }
}

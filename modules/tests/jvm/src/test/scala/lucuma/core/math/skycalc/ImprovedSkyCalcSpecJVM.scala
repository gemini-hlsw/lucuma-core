// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import munit.ScalaCheckSuite
import org.scalacheck.Prop._

import edu.gemini.skycalc.ImprovedSkyCalcTest
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates._
import lucuma.core.math.arb.ArbPlace._
import com.fortysevendeg.scalacheck.datetime.instances.jdk8._
import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import java.time._
import lucuma.core.math.Place
import jsky.coords.WorldCoords
import java.{ util => ju }
import org.scalacheck.{ Test => ScalaCheckTest }

final class ImprovedSkyCalcSpecJVM extends ScalaCheckSuite {

  override protected val scalaCheckTestParameters = {
    val old = ScalaCheckTest.Parameters.default
    old.withMinSuccessfulTests(old.minSuccessfulTests / 3) // this is a slow test
  }

  private val zdtFrom  = ZonedDateTime.of(
    LocalDate.of(1901, 1, 1),
    LocalTime.MIDNIGHT,
    ZoneOffset.UTC
  )
  private val zdtRange = Duration.ofDays(Period.ofYears(1000).getDays.toLong)

  test("ImprovedSkyCalcSpec: Arbitrary sky calculations") {
    forAll { place: Place =>
      // This SkyCalc should be thread safe, but Java's isn't.
      val calc = ImprovedSkyCalc(place)

      // This generator already provides ZDTs with millisecond precision, not nano.
      forAll(genDateTimeWithinRange(zdtFrom, zdtRange)) { zdt =>
        val instant = zdt.toInstant

        forAll { coords: Coordinates =>
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

          assertEquals(results.altitudeRaw, javaCalc.getAltitude)
          assertEquals(results.azimuthRaw, javaCalc.getAzimuth)
          assertEquals(results.parallacticAngleRaw, javaCalc.getParallacticAngle)
          assertEquals(results.hourAngleRaw, javaCalc.getHourAngle)
          assertEquals(results.lunarIlluminatedFraction, javaCalc.getLunarIlluminatedFraction)
          assertEquals(results.lunarSkyBrightness, javaCalc.getLunarSkyBrightness)
          assertEquals(results.totalSkyBrightness, javaCalc.getTotalSkyBrightness)
          assertEquals(results.lunarPhaseAngleRaw, javaCalc.getLunarPhaseAngle)
          assertEquals(results.sunAltitudeRaw, javaCalc.getSunAltitude)
          assertEquals(results.lunarDistance, javaCalc.getLunarDistance)
          assertEquals(results.lunarElevationRaw, javaCalc.getLunarElevation)
        }
      }
    }
  }
}

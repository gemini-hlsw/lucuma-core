// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import com.fortysevendeg.scalacheck.datetime.instances.jdk8.*
import edu.gemini.skycalc.ImprovedSkyCalcTest
import jsky.coords.WorldCoords
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Place
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.arb.ArbPlace.given
import lucuma.core.model.ObjectTracking
import lucuma.core.tests.ScalaCheckFlaky
import lucuma.core.util.TimeSpan
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Test as ScalaCheckTest

import java.time.*
import java.util as ju

final class ImprovedSkyCalcSuiteJVM extends ScalaCheckSuite {

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

  test("average parallactic angle no visible target") {
    val when = Instant.parse("2024-08-02T21:00:00Z")
    val coord = Coordinates.fromHmsDms.getOption("00:15:21.550600 +28:35:13.831766").get
    assert(averageParallacticAngle(
      Site.GN.place,
      ObjectTracking.constant(coord),
      when,
      TimeSpan.fromHours(1).get).isEmpty
    )
  }

  test("Arbitrary sky calculations".tag(ScalaCheckFlaky)) {
    forAll { (place: Place) =>
      // This SkyCalc should be thread safe, but Java's isn't.
      val calc = ImprovedSkyCalc(place)

      // This generator already provides ZDTs with millisecond precision, not nano.
      forAll(genDateTimeWithinRange(zdtFrom, zdtRange)) { zdt =>
        val instant = zdt.toInstant

        forAll { (coords: Coordinates) =>
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

          // We use constants with more precision
          assertEqualsDouble(results.altitudeRaw, javaCalc.getAltitude,  1e-12)
          assertEqualsDouble(results.azimuthRaw, javaCalc.getAzimuth, 1e-12)
          assertEqualsDouble(results.parallacticAngleRaw, javaCalc.getParallacticAngle, 1e-12)
          assertEqualsDouble(results.hourAngleRaw, javaCalc.getHourAngle, 1e-12)
          assertEqualsDouble( results.lunarIlluminatedFraction.toDouble, javaCalc.getLunarIlluminatedFraction.toDouble, 1e-12)
          assertEqualsDouble(results.lunarSkyBrightness, javaCalc.getLunarSkyBrightness, 1e-12)
          assertEqualsDouble(results.totalSkyBrightness, javaCalc.getTotalSkyBrightness, 1e-12)
          assertEqualsDouble(results.lunarPhaseAngleRaw, javaCalc.getLunarPhaseAngle, 1e-12)
          assertEqualsDouble(results.sunAltitudeRaw, javaCalc.getSunAltitude, 1e-12)
          assertEqualsDouble(results.lunarDistance, javaCalc.getLunarDistance, 1e-12)
          assertEqualsDouble(results.lunarElevationRaw, javaCalc.getLunarElevation, 1e-12)
        }
      }
    }
  }
}

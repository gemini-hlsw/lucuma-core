// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import com.fortysevendeg.scalacheck.datetime.GenDateTime.genDateTimeWithinRange
import com.fortysevendeg.scalacheck.datetime.instances.jdk8._
import edu.gemini.skycalc.ImprovedSkyCalcTest
import jsky.coords.WorldCoords
import lucuma.core.math.Coordinates
import lucuma.core.math.Place
import lucuma.core.math.arb.ArbCoordinates._
import lucuma.core.math.arb.ArbPlace._
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.{ Test => ScalaCheckTest }
import org.scalactic.Tolerance

import java.time._
import java.{ util => ju }

final class ImprovedSkyCalcSuiteJVM extends ScalaCheckSuite with Tolerance {

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

  test("Arbitrary sky calculations") {
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

          // We use constants with more precision
          assert((results.altitudeRaw +- 1e-12).isWithin(javaCalc.getAltitude))
          assert((results.azimuthRaw +- 1e-12).isWithin(javaCalc.getAzimuth))
          assert((results.parallacticAngleRaw +- 1e-12).isWithin(javaCalc.getParallacticAngle))
          assert((results.hourAngleRaw +- 1e-12).isWithin(javaCalc.getHourAngle))
          assert(
            (results.lunarIlluminatedFraction.toDouble +- 1e-12).isWithin(
              javaCalc.getLunarIlluminatedFraction.toDouble
            )
          )
          assert((results.lunarSkyBrightness +- 1e-12).isWithin(javaCalc.getLunarSkyBrightness))
          assert((results.totalSkyBrightness +- 1e-12).isWithin(javaCalc.getTotalSkyBrightness))
          assert((results.lunarPhaseAngleRaw +- 1e-12).isWithin(javaCalc.getLunarPhaseAngle))
          assert((results.sunAltitudeRaw +- 1e-12).isWithin(javaCalc.getSunAltitude))
          assert((results.lunarDistance +- 1e-12).isWithin(javaCalc.getLunarDistance))
          assert((results.lunarElevationRaw +- 1e-12).isWithin(javaCalc.getLunarElevation))
        }
      }
    }
  }
}

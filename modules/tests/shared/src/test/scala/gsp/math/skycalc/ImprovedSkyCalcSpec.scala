// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import weaver._

import cats.Monoid
import cats.Show
import java.time._
import gsp.math.Coordinates
import gsp.math.Angle

// This is just a basic case, mostly to test linking in JS.
// Property based testing is in ImprovedSkyCalcSpecJVM, where output
// is compared to the one from {edu.gemini.skycalc} in Java.
object ImprovedSkyCalcSpec extends SimpleIOSuite {

  implicit val showInstant: Show[Instant]   = Show.fromToString
  implicit val showZDT: Show[ZonedDateTime] = Show.fromToString

  private val GN = (Angle.fromDoubleDegrees(19.8238068), Angle.fromDoubleDegrees(-155.4690550), 4213)
  private val GS = (Angle.fromDoubleDegrees(-30.2407494), Angle.fromDoubleDegrees(-70.7366867), 2722)
  private val M51 = Coordinates.fromHmsDms.getOption("13 29 52.698000 +47 11 42.929988").get
  private val Moment  = ZonedDateTime.of(LocalDate.of(2000,1,1), LocalTime.MIDNIGHT, ZoneOffset.UTC).toInstant

  private val expected: Map[((Angle, Angle, Int), Coordinates, Instant), Double] =
    Map(
      (GN, M51, Moment) -> 6.637492164370325,
      (GS, M51, Moment) -> -72.26086414074315
    )

  pureTest("ImprovedSkyCalcSpec: Elevation of M51 at midnight 2000-01-01 UTC") {
    Monoid[Expectations].combineAll(
      expected.map{ case (((lat, long, alt), coords, instant), elevation) =>
        val calc     = new ImprovedSkyCalc(lat, long, alt)
        calc.calculate(coords, instant, false)
        expect(calc.getAltitude == elevation)
      }
    )
  }
}

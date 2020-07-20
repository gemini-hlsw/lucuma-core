// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import weaver._

import cats.implicits._
import cats.Monoid
import cats.Show
import java.time._
import gsp.math.Coordinates
import gsp.math.Place

// This is just a basic case, mostly to test linking in JS.
// Property based testing is in ImprovedSkyCalcSpecJVM, where output
// is compared to the one from {edu.gemini.skycalc} in Java.
object ImprovedSkyCalcSpec extends SimpleIOSuite {

  implicit val showInstant: Show[Instant]   = Show.fromToString
  implicit val showZDT: Show[ZonedDateTime] = Show.fromToString

  private val NanosPerMillis: Int = 1_000_000

  private def truncateInstantToMillis(i: Instant): Instant =
    Instant.ofEpochSecond(
      i.getEpochSecond,
      (i.getNano / NanosPerMillis * NanosPerMillis).toLong
    )

  private val M51    = Coordinates.fromHmsDms.getOption("13 29 52.698000 +47 11 42.929988").get
  private val Moment = truncateInstantToMillis(
    ZonedDateTime.of(LocalDate.of(2000, 1, 1), LocalTime.MIDNIGHT, ZoneOffset.UTC).toInstant
  )

  // Known results with OCS, computed with millis precision (uses ju.Date)
  private val expected: Map[(Place, Coordinates, Instant), Double] =
    Map(
      (GN, M51, Moment) -> 6.637492164341347,
      (GS, M51, Moment) -> -72.26086414073282
    )

  pureTest("ImprovedSkyCalcSpec: Elevation of M51 at midnight 2000-01-01 UTC") {
    Monoid[Expectations].combineAll(
      expected.map {
        case ((place, coords, instant), elevation) =>
          val calc    = ImprovedSkyCalc(place)
          val results = calc.calculate(coords, instant, false)
          expect(results.altitudeRaw === elevation)
      }
    )
  }
}

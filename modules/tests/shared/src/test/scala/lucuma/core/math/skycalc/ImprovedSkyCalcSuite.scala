// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import lucuma.core.enums.Site
import lucuma.core.math.Constants.SiderealRate
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates.given
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import java.time.*

// This is just a basic case, mostly to test linking in JS.
// Property based testing is in ImprovedSkyCalcSpecJVM, where output
// is compared to the one from {edu.gemini.skycalc} in Java.
final class ImprovedSkyCalcSuite extends ScalaCheckSuite {

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
  private val expected: Map[(Site, Coordinates, Instant), Double] =
    Map(
      (Site.GN, M51, Moment) -> 6.637492164341347,
      (Site.GS, M51, Moment) -> -72.26086414073282
    )

  test("Elevation of M51 at midnight 2000-01-01 UTC") {
    expected.foreach {
      case ((site, coords, instant), elevation) =>
        val calc    = ImprovedSkyCalc(site.place)
        val results = calc.calculate(coords, instant, false)
        // We use constants with more precision
        assertEqualsDouble(results.altitudeRaw, elevation, 1e-12)
    }
  }

  private val OneSecondInHours: Double = 1.0 / 3600.0

  private val SiderealDay: Duration = Duration.ofMillis(Math.round(24.0 * 3600.0 * 1000.0 / SiderealRate))

  private val siteGen: Gen[Site] = Gen.oneOf(Site.GN, Site.GS)

  private val instantGen: Gen[Instant] =
    Gen
      .choose(
        Instant.parse("1950-01-01T00:00:00Z").toEpochMilli,
        Instant.parse("2100-01-01T00:00:00Z").toEpochMilli
      )
      .map(Instant.ofEpochMilli)

  private def hourAngleHoursAt(site: Site, coords: Coordinates, instant: Instant): Double =
    ImprovedSkyCalc(site.place).calculate(coords, instant, false).hourAngleRaw

  private def assertTransit(site: Site, coords: Coordinates, after: Instant, transit: Instant): Unit = {
    assert(transit.isAfter(after), s"$transit is not after $after")
    assert(!transit.isAfter(after.plus(Duration.ofDays(1))), s"$transit is more than 24h after $after")
    assertEqualsDouble(hourAngleHoursAt(site, coords, transit), 0.0, OneSecondInHours)
  }

  test("Next transit of M51 after midnight 2000-01-01 UTC") {
    List(Site.GN, Site.GS).foreach { site =>
      val transit: Instant = ImprovedSkyCalc.nextTransit(site, M51, Moment)
      assertTransit(site, M51, Moment, transit)
    }
  }

  test("Next transit after a transit is one sidereal day later") {
    List(Site.GN, Site.GS).foreach { site =>
      val firstTransit: Instant  = ImprovedSkyCalc.nextTransit(site, M51, Moment)
      val secondTransit: Instant = ImprovedSkyCalc.nextTransit(site, M51, firstTransit)
      assertTransit(site, M51, firstTransit, secondTransit)
      val separationDifferenceMillis: Long =
        Duration.between(firstTransit, secondTransit).minus(SiderealDay).toMillis.abs
      assert(separationDifferenceMillis <= 3000L, s"Off by $separationDifferenceMillis ms")
    }
  }

  test("Next transit shortly before a transit is that transit") {
    List(Site.GN, Site.GS).foreach { site =>
      val transit: Instant       = ImprovedSkyCalc.nextTransit(site, M51, Moment)
      val oneHourBefore: Instant = transit.minus(Duration.ofHours(1))
      val found: Instant         = ImprovedSkyCalc.nextTransit(site, M51, oneHourBefore)
      assert(Duration.between(transit, found).toMillis.abs <= 1000L, s"$found differs from $transit")
    }
  }

  property("Next transit is within 24h and at hour angle zero") {
    forAll(siteGen, arbitrary[Coordinates], instantGen) {
      (site: Site, coords: Coordinates, after: Instant) =>
        assertTransit(site, coords, after, ImprovedSkyCalc.nextTransit(site, coords, after))
    }
  }
}

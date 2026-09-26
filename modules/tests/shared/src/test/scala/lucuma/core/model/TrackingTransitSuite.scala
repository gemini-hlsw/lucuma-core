// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.arb.ArbSiderealTracking.given
import lucuma.core.util.Timestamp
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

import java.time.Duration
import java.time.Instant

final class TrackingTransitSuite extends ScalaCheckSuite:

  private val OneSecondInHours: Double = 1.0 / 3600.0

  private val siteGen: Gen[Site] = Gen.oneOf(Site.GN, Site.GS)

  private val instantGen: Gen[Instant] =
    Gen
      .choose(
        Instant.parse("1950-01-01T00:00:00Z").toEpochMilli,
        Instant.parse("2100-01-01T00:00:00Z").toEpochMilli
      )
      .map(Instant.ofEpochMilli)

  private val Now: Instant = Instant.parse("2024-06-01T00:00:00Z")

  private def coordinates(hmsDms: String): Coordinates =
    Coordinates.fromHmsDms.getOption(hmsDms).get

  private def ephemerisElement(offsetFromNow: Duration, hmsDms: String): EphemerisTracking.Element =
    (Timestamp.unsafeFromInstant(Now.plus(offsetFromNow)), EphemerisCoordinates(coordinates(hmsDms), Offset.Zero))

  private def hoursFromMeridian(site: Site, coords: Coordinates, instant: Instant): Double =
    val hourAngleHours: Double =
      ImprovedSkyCalc(site.place).calculate(coords, instant, false).hourAngle.toDoubleHours
    hourAngleHours.min(24.0 - hourAngleHours)

  property("Sidereal tracking transit matches the primitive within proper motion drift"):
    forAll(siteGen, arbitrary[SiderealTracking], instantGen):
      (site: Site, tracking: SiderealTracking, after: Instant) =>
        val expected: Option[Instant] =
          tracking.at(after).map(ImprovedSkyCalc.nextTransit(site, _, after))
        (tracking.nextTransit(site, after), expected) match
          case (Some(transit), Some(primitive)) =>
            assert(Duration.between(primitive, transit).abs.compareTo(Duration.ofSeconds(2)) <= 0)
          case (None, None)                     => ()
          case (transit, primitive)             => fail(s"Got $transit, expected $primitive")

  property("Constant tracking transit equals the primitive"):
    forAll(siteGen, arbitrary[Coordinates], instantGen):
      (site: Site, coords: Coordinates, after: Instant) =>
        assertEquals(
          ConstantTracking(coords).nextTransit(site, after),
          Some(ImprovedSkyCalc.nextTransit(site, coords, after))
        )

  property("timeOrNextTransit prefers the explicit time"):
    forAll(siteGen, arbitrary[Coordinates], instantGen, Gen.option(instantGen)):
      (site: Site, coords: Coordinates, now: Instant, explicitTime: Option[Instant]) =>
        val tracking: Tracking = ConstantTracking(coords)
        assertEquals(
          tracking.timeOrNextTransit(site, explicitTime, now),
          explicitTime.orElse(tracking.nextTransit(site, now))
        )
        explicitTime.foreach(time => assertEquals(tracking.timeOrNextTransit(site, Some(time), now), Some(time)))

  test("timeOrNextTransit without explicit time is the next transit"):
    val tracking: Tracking = ConstantTracking(coordinates("13 29 52.698 +47 11 42.93"))
    assertEquals(
      tracking.timeOrNextTransit(Site.GN, None, Now),
      Some(ImprovedSkyCalc.nextTransit(Site.GN, coordinates("13 29 52.698 +47 11 42.93"), Now))
    )

  test("Ephemeris not covering the start has no transit"):
    val ephemeris: EphemerisTracking =
      EphemerisTracking(
        ephemerisElement(Duration.ofDays(1), "10 00 00.0 +10 00 00.0"),
        ephemerisElement(Duration.ofDays(2), "10 04 00.0 +10 30 00.0")
      )
    assertEquals(ephemeris.nextTransit(Site.GN, Now), None)

  test("Ephemeris covering a day around the start transits at hour angle zero"):
    val ephemeris: EphemerisTracking =
      EphemerisTracking(
        ephemerisElement(Duration.ofHours(-12), "10 00 00.0 +10 00 00.0"),
        ephemerisElement(Duration.ofHours(12), "10 04 00.0 +10 30 00.0"),
        ephemerisElement(Duration.ofHours(36), "10 08 00.0 +11 00 00.0")
      )
    List(Site.GN, Site.GS).foreach: site =>
      val transit: Instant = ephemeris.nextTransit(site, Now).get
      assert(transit.isAfter(Now))
      assert(!transit.isAfter(Now.plus(Duration.ofDays(1))))
      assertEqualsDouble(hoursFromMeridian(site, ephemeris.at(transit).get, transit), 0.0, OneSecondInHours)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt

class HorizonsClientEphemerisSuite extends HorizonsClientSuite:

  private val site = Site.GN
  private val sem  = Semester(YearInt.unsafeFrom(2020), Half.B)
  private val elems = 10

  def fetchEphemeris(key: EphemerisKey.Horizons): IO[Either[String, List[HorizonsEphemerisEntry]]] =
    client.use: c =>
      c.ephemeris(
        key   = key,
        site  = site,
        start = sem.start.atSite(site).toInstant,
        stop  = sem.end.atSite(site).toInstant,
        elems = elems
      ).map(_.map(_.entries))

  def testEphemerisPopulation(name: String, key: EphemerisKey.Horizons) =
    test(s"Ensure ephemeris is populated for $site - $name (${key.keyType})"):
      assertIOBoolean:
        fetchEphemeris(key).map(e => (e.toOption.get.length - elems).abs < 3)

  def testEphemerisEmpty(key: EphemerisKey.Horizons) =
    test(s"Ensure ephemeris is empty for $site - ${key.des} (${key.keyType})"):
      assertIOBoolean:
        fetchEphemeris(key).map(_.toOption.get.isEmpty)

  testEphemerisPopulation("Hally", EphemerisKey.Comet("1P"))
  testEphemerisPopulation("Sedna", EphemerisKey.AsteroidNew("2003 VB12"))
  testEphemerisPopulation("'Oumuamua", EphemerisKey.AsteroidNew("A/2017 U1"))
  testEphemerisPopulation("Amphitrite", EphemerisKey.AsteroidOld(29))
  testEphemerisPopulation("Charon", EphemerisKey.MajorBody(901))
  
  testEphemerisEmpty(EphemerisKey.Comet("17653287465t4"))
  testEphemerisEmpty(EphemerisKey.AsteroidNew("17653287465t4"))
  testEphemerisEmpty(EphemerisKey.AsteroidNew("17653287465t4"))
  testEphemerisEmpty(EphemerisKey.AsteroidOld(88715673))
  testEphemerisEmpty(EphemerisKey.MajorBody(85732756))

  test("Ensure ephemeris content is correct (Halley)"):
    assertIO(
      fetchEphemeris(EphemerisKey.Comet("1P")),
      s"""|2020-Aug-01 00:00:00.000 *   08 22 21.332241 +02 50 35.97409  3.742927  -0.97977   1.189  0.130   25.628  29.111
          |2020-Aug-19 09:35:00.000     08 24 09.845849 +02 42 20.15022  3.565667  -1.26261    n.a.   n.a.   25.626  29.115
          |2020-Sep-06 19:10:00.000 *m  08 25 44.746731 +02 32 22.49657  2.819498  -1.43290   1.058  0.115   25.618  29.117
          |2020-Sep-25 04:45:00.000 Nm  08 26 56.773006 +02 21 37.23421  2.011780  -1.47332    n.a.   n.a.   25.605  29.114
          |2020-Oct-13 14:20:00.000  m  08 27 38.048238 +02 11 06.21859  0.762946  -1.36484   1.475  0.161   25.590  29.104
          |2020-Oct-31 23:55:00.000 *   08 27 42.939875 +02 01 53.56865  -0.45926  -1.11532    n.a.   n.a.   25.572  29.088
          |2020-Nov-19 09:30:00.000     08 27 09.281733 +01 55 00.79745  -1.80543  -0.72960   6.129  0.667   25.554  29.068
          |2020-Dec-07 19:05:00.000 *m  08 25 58.630753 +01 51 19.10674  -2.97630  -0.25527   6.065  0.661   25.538  29.044
          |2020-Dec-26 04:40:00.000 Nm  08 24 17.223418 +01 51 20.35700  -3.83070  0.274248    n.a.   n.a.   25.525  29.022
          |2021-Jan-13 14:15:00.000     08 22 15.210694 +01 55 11.20811  -4.39315  0.777934   1.496  0.163   25.518  29.003
          |2021-Jan-31 23:50:00.000 *   08 20 05.764858 +02 02 30.25705  -4.30205  1.205358    n.a.   n.a.   25.517  28.999
          |""".stripMargin.linesIterator.toList.traverse(HorizonsParser.parseEntry)
      )

  test("Stop must fall after start."):
    assertIO(
      client.use: c =>
        c.ephemeris(
          key   = EphemerisKey.Comet("1P"),
          site  = site,
          start = sem.start.atSite(site).toInstant,
          stop  = sem.start.atSite(site).toInstant,
          elems = elems
        ),
      Left("Stop must fall after start.")
    )

  test("Cannot select fewer than one element."):
    assertIO(
      client.use: c =>
        c.ephemeris(
          key   = EphemerisKey.Comet("1P"),
          site  = site,
          start = sem.start.atSite(site).toInstant,
          stop  = sem.end.atSite(site).toInstant,
          elems = 0
        ),
      Left("Cannot select fewer than one element.")
    )

  test("alignedEphemeris (2)"):
    assertIO(
      client.use: c =>

        c.alignedEphemeris(
          key   = EphemerisKey.Comet("1P"),
          site  = site,
          start = sem.start.atSite(site).toInstant,
          days  = 10,
          cadence = 2
        ).map: e =>
          e.map: eph =>            
            eph.entries.map(_.when.toString),
        
      Right(List(
        "2020-08-01T00:00:00Z", // Day 1, midnight
        "2020-08-01T12:00:00Z", // Day 1, noon
        "2020-08-02T00:00:00Z", // Day 2
        "2020-08-02T12:00:00Z", 
        "2020-08-03T00:00:00Z",
        "2020-08-03T12:00:00Z",
        "2020-08-04T00:00:00Z", // ...
        "2020-08-04T12:00:00Z",
        "2020-08-05T00:00:00Z",
        "2020-08-05T12:00:00Z",
        "2020-08-06T00:00:00Z",
        "2020-08-06T12:00:00Z",
        "2020-08-07T00:00:00Z",
        "2020-08-07T12:00:00Z",
        "2020-08-08T00:00:00Z",
        "2020-08-08T12:00:00Z",
        "2020-08-09T00:00:00Z",
        "2020-08-09T12:00:00Z",
        "2020-08-10T00:00:00Z", // Day 10
        "2020-08-10T12:00:00Z", // Day 10, noon
        "2020-08-11T00:00:00Z"  // Day 11, midnight ... we get one extra
      ))
    )

  test("alignedEphemeris (4)"):
    assertIO(
      client.use: c =>

        c.alignedEphemeris(
          key   = EphemerisKey.Comet("1P"),
          site  = site,
          start = sem.start.atSite(site).toInstant,
          days  = 10,
          cadence = 4
        ).map: e =>
          e.map: eph =>            
            eph.entries.map(_.when.toString),
        
      Right(List(
        "2020-08-01T00:00:00Z",
        "2020-08-01T06:00:00Z",
        "2020-08-01T12:00:00Z",
        "2020-08-01T18:00:00Z",
        "2020-08-02T00:00:00Z",
        "2020-08-02T06:00:00Z",
        "2020-08-02T12:00:00Z",
        "2020-08-02T18:00:00Z",
        "2020-08-03T00:00:00Z",
        "2020-08-03T06:00:00Z",
        "2020-08-03T12:00:00Z",
        "2020-08-03T18:00:00Z",
        "2020-08-04T00:00:00Z",
        "2020-08-04T06:00:00Z",
        "2020-08-04T12:00:00Z",
        "2020-08-04T18:00:00Z",
        "2020-08-05T00:00:00Z",
        "2020-08-05T06:00:00Z",
        "2020-08-05T12:00:00Z",
        "2020-08-05T18:00:00Z",
        "2020-08-06T00:00:00Z",
        "2020-08-06T06:00:00Z",
        "2020-08-06T12:00:00Z",
        "2020-08-06T18:00:00Z",
        "2020-08-07T00:00:00Z",
        "2020-08-07T06:00:00Z",
        "2020-08-07T12:00:00Z",
        "2020-08-07T18:00:00Z",
        "2020-08-08T00:00:00Z",
        "2020-08-08T06:00:00Z",
        "2020-08-08T12:00:00Z",
        "2020-08-08T18:00:00Z",
        "2020-08-09T00:00:00Z",
        "2020-08-09T06:00:00Z",
        "2020-08-09T12:00:00Z",
        "2020-08-09T18:00:00Z",
        "2020-08-10T00:00:00Z",
        "2020-08-10T06:00:00Z",
        "2020-08-10T12:00:00Z",
        "2020-08-10T18:00:00Z",
        "2020-08-11T00:00:00Z"
      ))
    )

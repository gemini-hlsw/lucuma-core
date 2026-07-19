// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.effect.*
import cats.syntax.all.*
import lucuma.core.enums.Instrument
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.syntax.int.*
import munit.CatsEffectSuite

class GoaClientSuite extends CatsEffectSuite:

  val testCoords: Coordinates = Coordinates(
    RightAscension.fromDoubleDegrees(182.64),
    Declination.fromDoubleDegrees(30.40166667).get
  )

  val searchRadius: Angle = 60.arcseconds

  test("GoaClientMock.fromResource loads and parses JSON"):
    GoaClientMock
      .fromResource[IO]("goa/sidereal-response.json")
      .flatMap: client =>
        val params = GoaParams.Sidereal(testCoords, Instrument.GmosNorth, searchRadius)

        client
          .query(params)
          .map: result =>
            assert(result.isRight, s"Query failed: ${result.left.getOrElse("")}")
            val records = result.toOption.get
            assertEquals(records.length, 2)
            assertEquals(records.head.programId, Some("GN-2024A-Q-101"))

  test("GoaClient.noop returns empty list"):
    val client = GoaClient.noop[IO]
    val params = GoaParams.Sidereal(testCoords, Instrument.GmosNorth, searchRadius)

    client
      .query(params)
      .map: result =>
        assert(result.isRight)
        assertEquals(result.toOption.get, List.empty)

  test("GoaClientMock returns error for unsupported instrument"):
    val client = GoaClientMock.empty[IO]
    val params = GoaParams.Sidereal(testCoords, Instrument.VisitorNorth, searchRadius)

    client
      .query(params)
      .map: result =>
        assert(result.isLeft)
        val errors = result.left.toOption.get.toList
        assertEquals(errors.length, 1)
        assertMatches(errors.head) { case GoaQueryError.UnsupportedInstrument(_) =>
          true
        }

  test("GoaInstrument maps all supported instruments"):
    assertEquals(GoaInstrument.toGoaName(Instrument.GmosNorth), Some("GMOS-N"))
    assertEquals(GoaInstrument.toGoaName(Instrument.GmosSouth), Some("GMOS-S"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Flamingos2), Some("F2"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Gnirs), Some("GNIRS"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Niri), Some("NIRI"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Ghost), Some("GHOST"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Gpi), Some("GPI"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Gsaoi), Some("GSAOI"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Alopeke), Some("ALOPEKE"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Zorro), Some("ZORRO"))
    assertEquals(GoaInstrument.toGoaName(Instrument.Igrins2), Some("IGRINS2"))

  test("GoaInstrument returns None for unsupported instruments"):
    assertEquals(GoaInstrument.toGoaName(Instrument.VisitorNorth), None)
    assertEquals(GoaInstrument.toGoaName(Instrument.AcqCamNorth), None)
    assertEquals(GoaInstrument.toGoaName(Instrument.Scorpio), None)

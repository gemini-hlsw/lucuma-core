// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import cats.effect.*
import fs2.io.readClassLoaderResource
import fs2.text
import io.circe.parser.decode
import munit.CatsEffectSuite

import java.time.Instant
import java.time.LocalDate

class GoaCodecsSuite extends CatsEffectSuite:

  import codecs.given

  def loadResource(name: String): IO[String] =
    readClassLoaderResource[IO](name, 8192)
      .through(text.utf8.decode)
      .compile
      .string

  test("decode sidereal response JSON"):
    loadResource("goa/sidereal-response.json").map: json =>
      val result  = decode[List[GoaSummaryRecord]](json)
      assert(result.isRight, s"Failed to decode: ${result.left.getOrElse("")}")
      val records = result.toOption.get
      assertEquals(records.length, 2)

      val first = records.head
      assertEquals(first.name, "N20240115S0001.fits")
      assertEquals(first.dataLabel, Some("GN-2024A-Q-101-1-001"))
      assertEquals(first.ra, Some(182.64))
      assertEquals(first.dec, Some(30.40166667))
      assertEquals(first.instrument, "GMOS-N")
      assertEquals(first.observationType, "OBJECT")
      assertEquals(first.observationClass, Some("science"))
      assertEquals(first.qaState, Some("Pass"))
      assertEquals(first.utDateTime, Some(Instant.parse("2024-01-15T06:42:29.400Z")))
      assertEquals(first.releaseDate, Some(LocalDate.of(2025, 7, 15)))
      assertEquals(first.programId, Some("GN-2024A-Q-101"))
      assertEquals(first.observationId, Some("GN-2024A-Q-101-1"))
      assertEquals(first.objectName, Some("NGC4150"))
      assertEquals(first.exposure, Some(1200.0))
      assertEquals(first.disperser, Some("B600"))
      assertEquals(first.filter, Some("g"))
      assertEquals(first.wavelength, Some(0.475))
      assertEquals(first.airmass, Some(1.126))

  test("decode nonsidereal response JSON"):
    loadResource("goa/nonsidereal-response.json").map: json =>
      val result  = decode[List[GoaSummaryRecord]](json)
      assert(result.isRight, s"Failed to decode: ${result.left.getOrElse("")}")
      val records = result.toOption.get
      assertEquals(records.length, 1)

      val record = records.head
      assertEquals(record.name, "S20230520S0045.fits")
      assertEquals(record.objectName, Some("Ceres"))
      assertEquals(record.instrument, "GMOS-S")
      assertEquals(record.qaState, Some("Usable"))

  test("decode empty response JSON"):
    val json   = "[]"
    val result = decode[List[GoaSummaryRecord]](json)
    assert(result.isRight)
    assertEquals(result.toOption.get, List.empty)

  test("decode response with null fields"):
    val json =
      """[{
        |  "name": "test.fits",
        |  "instrument": "GMOS-N",
        |  "observation_type": "OBJECT",
        |  "data_label": null,
        |  "ra": null,
        |  "dec": null,
        |  "observation_class": null,
        |  "qa_state": null,
        |  "ut_datetime": null,
        |  "release": null,
        |  "program_id": null,
        |  "observation_id": null,
        |  "object": null,
        |  "exposure_time": null,
        |  "disperser": null,
        |  "filter_name": null,
        |  "central_wavelength": null,
        |  "airmass": null,
        |  "azimuth": null,
        |  "elevation": null
        |}]""".stripMargin

    val result  = decode[List[GoaSummaryRecord]](json)
    assert(result.isRight, s"Failed to decode: ${result.left.getOrElse("")}")
    val records = result.toOption.get
    assertEquals(records.length, 1)
    assertEquals(records.head.name, "test.fits")
    assertEquals(records.head.dataLabel, None)
    assertEquals(records.head.ra, None)

  test("Instant decoder handles various formats and assumes UTC"):
    // 6-digit fraction, no offset -> UTC
    val json1   =
      """{"name": "test.fits", "instrument": "GMOS-N", "observation_type": "OBJECT", "ut_datetime": "2024-01-15 06:42:29.400000"}"""
    val result1 = decode[GoaSummaryRecord](json1)
    assert(result1.isRight)
    assertEquals(result1.toOption.get.utDateTime, Some(Instant.parse("2024-01-15T06:42:29.400Z")))

    // no fraction -> UTC
    val json2   =
      """{"name": "test.fits", "instrument": "GMOS-N", "observation_type": "OBJECT", "ut_datetime": "2024-01-15 06:42:29"}"""
    val result2 = decode[GoaSummaryRecord](json2)
    assert(result2.isRight)
    assertEquals(result2.toOption.get.utDateTime, Some(Instant.parse("2024-01-15T06:42:29Z")))

    // single-digit fraction -> tolerated
    val json3   =
      """{"name": "test.fits", "instrument": "GMOS-N", "observation_type": "OBJECT", "ut_datetime": "2024-01-15 06:42:29.4"}"""
    val result3 = decode[GoaSummaryRecord](json3)
    assert(result3.isRight, s"Failed to decode: ${result3.left.getOrElse("")}")
    assertEquals(result3.toOption.get.utDateTime, Some(Instant.parse("2024-01-15T06:42:29.400Z")))

    // explicit +00:00 offset -> honored
    val json4   =
      """{"name": "test.fits", "instrument": "GMOS-N", "observation_type": "OBJECT", "ut_datetime": "2024-01-15 06:42:29.400000+00:00"}"""
    val result4 = decode[GoaSummaryRecord](json4)
    assert(result4.isRight, s"Failed to decode: ${result4.left.getOrElse("")}")
    assertEquals(result4.toOption.get.utDateTime, Some(Instant.parse("2024-01-15T06:42:29.400Z")))

    // non-zero offset -> converted to UTC instant
    val json5   =
      """{"name": "test.fits", "instrument": "GMOS-N", "observation_type": "OBJECT", "ut_datetime": "2024-01-15 06:42:29-05:00"}"""
    val result5 = decode[GoaSummaryRecord](json5)
    assert(result5.isRight, s"Failed to decode: ${result5.left.getOrElse("")}")
    assertEquals(result5.toOption.get.utDateTime, Some(Instant.parse("2024-01-15T11:42:29Z")))

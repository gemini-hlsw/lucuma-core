// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import java.time.Instant
import java.time.LocalDateTime

final class TerrestrialInstantSuite extends munit.DisciplineSuite {
  test("UTC -> TT for January 1st, noon"):
    val testCases = List(
      (1975, "1975-01-01T12:00:46.184"),
      (1980, "1980-01-01T12:00:51.184"),
      (1985, "1985-01-01T12:00:54.184"),
      (1990, "1990-01-01T12:00:57.184"),
      (1995, "1995-01-01T12:01:01.184")
    )

    testCases.foreach: (year, expected) =>
      val ldt: LocalDateTime   = LocalDateTime.of(year, 1, 1, 12, 0, 0)
      val tt: LocalDateTime = LocalDateTime.parse(expected)
      val ti: TerrestrialInstant = TerrestrialInstant.unsafeFromInstant(ldt.toInstant(java.time.ZoneOffset.UTC))

      assertEquals(ti.value, tt.toInstant(java.time.ZoneOffset.UTC))

  test("TT -> UTC for January 1st, noon"):
    val testCases = List(
      (1975, "1975-01-01T11:59:13.816"),
      (1980, "1980-01-01T11:59:08.816"),
      (1985, "1985-01-01T11:59:05.816"),
      (1990, "1990-01-01T11:59:02.816"),
      (1995, "1995-01-01T11:58:58.816"),
      (2000, "2000-01-01T11:58:55.816"),
      (2005, "2005-01-01T11:58:55.816"),
      (2010, "2010-01-01T11:58:53.816"),
      (2015, "2015-01-01T11:58:52.816"),
      (2020, "2020-01-01T11:58:50.816"),
      (2025, "2025-01-01T11:58:50.816")
    )

    testCases.foreach: (year, expected) =>
      val tt: TerrestrialInstant = 
        TerrestrialInstant:
          LocalDateTime.of(year, 1, 1, 12, 0, 0).toInstant(java.time.ZoneOffset.UTC)
      val ldt: LocalDateTime = LocalDateTime.parse(expected)
      val utcInstant: Instant = tt.unsafeToInstant

      assertEquals(utcInstant, ldt.toInstant(java.time.ZoneOffset.UTC))
}

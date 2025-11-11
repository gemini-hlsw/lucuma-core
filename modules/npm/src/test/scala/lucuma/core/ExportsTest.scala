// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

class ExportsTest extends munit.FunSuite {
  test("deg2hms") {
    assertNoDiff(deg2hms(0.0), "00:00:00.000")
    assertNoDiff(deg2hms(15.0), "01:00:00.000")
    assertNoDiff(deg2hms(-15.0), "23:00:00.000")
    assertNoDiff(deg2hms(23.4375), "01:33:45.000")
    // Test rounding
    assertNoDiff(deg2hms(359.999999), "00:00:00.000")
    assertNoDiff(deg2hms(0.000001), "00:00:00.000")
  }

  test("deg2dms") {
    assertNoDiff(deg2dms(0.0), "+00:00:00.00")
    assertNoDiff(deg2dms(15.0), "+15:00:00.00")
    assertNoDiff(deg2dms(-15.0), "-15:00:00.00")
    assertNoDiff(deg2dms(23.4375), "+23:26:15.00")
    // Test rounding
    assertNoDiff(deg2dms(-0.00001), "+00:00:00.00")
    assertNoDiff(deg2dms(0.00001), "+00:00:00.00")
  }

  test("hms2deg") {
    assertEquals(hms2deg("00:00:00.000"), 0.0)
    assertEquals(hms2deg("01:00:00.000"), 15.0)
    assertEquals(hms2deg("23:00:00.000"), 345.0)
    assertEquals(hms2deg("01:33:45.000"), 23.4375)
  }

  test("dms2deg") {
    assertEquals(dms2deg("+00:00:00.00"), 0.0)
    assertEquals(dms2deg("+15:00:00.00"), 15.0)
    assertEquals(dms2deg("-15:00:00.00"), 345d)
    assertEquals(dms2deg("+23:26:15.00"), 23.4375)
  }

  test("signedArcSeconds") {
    assertEquals(signedArcSeconds(360 * 60 * 60), 0d)
    assertEquals(signedArcSeconds(180 * 60 * 60), -648000d)
    assertEquals(signedArcSeconds(90 * 60 * 60), 324000d)
    assertEquals(signedArcSeconds(-90 * 60 * 60), -324000d)
    assertEquals(signedArcSeconds(270 * 60 * 60), -324000d)
  }
}

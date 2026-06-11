// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import scala.scalajs.js

class ExportsTest extends munit.FunSuite {
  test("deg2hms") {
    assertNoDiff(deg2hms(0.0), "00:00:00.000")
    assertNoDiff(deg2hms(15.0), "01:00:00.000")
    assertNoDiff(deg2hms(-15.0), "23:00:00.000")
    assertNoDiff(deg2hms(23.4375), "01:33:45.000")
    // Test rounding
    assertNoDiff(deg2hms(359.999999), "00:00:00.000")
    assertNoDiff(deg2hms(0.000001), "00:00:00.000")
    // Large values
    assertNoDiff(deg2hms(972000000000d), "09:52:03.652")
  }

  test("deg2dms") {
    assertNoDiff(deg2dms(0.0), "+00:00:00.00")
    assertNoDiff(deg2dms(15.0), "+15:00:00.00")
    assertNoDiff(deg2dms(-15.0), "-15:00:00.00")
    assertNoDiff(deg2dms(23.4375), "+23:26:15.00")
    // Test rounding
    assertNoDiff(deg2dms(-0.000001), "+00:00:00.00")
    assertNoDiff(deg2dms(0.000001), "+00:00:00.00")
    // Large values
    assertNoDiff(deg2dms(972000000000d), "+31:59:05.22")
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
    // String input
    assertEquals(signedArcSeconds("0"), 0d)
    assertEquals(signedArcSeconds("324000"), 324000d)
    assertEquals(signedArcSeconds("-324000"), -324000d)
  }

  test("dateToLocalObservingNight") {
    // At or after 14:00 local time the night belongs to the next calendar day
    assertNoDiff(dateToLocalObservingNight(new js.Date(2025, 0, 15, 20, 0, 0)), "2025-01-16")
    assertNoDiff(dateToLocalObservingNight(new js.Date(2025, 0, 15, 14, 0, 0)), "2025-01-16")
    // Before 14:00 local time the night belongs to the current calendar day
    assertNoDiff(dateToLocalObservingNight(new js.Date(2025, 0, 15, 2, 0, 0)), "2025-01-15")
    assertNoDiff(dateToLocalObservingNight(new js.Date(2025, 0, 15, 13, 59, 59)), "2025-01-15")
  }

  test("parseAttachmentId") {
    assertEquals(parseAttachmentId("a-1").toOption, Some("a-1"))
    assertEquals(parseAttachmentId("a-ff").toOption, Some("a-ff"))
    assertEquals(parseAttachmentId("invalid").toOption, None)
    assertEquals(parseAttachmentId("o-1").toOption, None)
  }

  test("parseCallForProposalsId") {
    assertEquals(parseCallForProposalsId("c-1").toOption, Some("c-1"))
    assertEquals(parseCallForProposalsId("c-abc").toOption, Some("c-abc"))
    assertEquals(parseCallForProposalsId("invalid").toOption, None)
    assertEquals(parseCallForProposalsId("a-1").toOption, None)
  }

  test("parseConfigurationRequestId") {
    assertEquals(parseConfigurationRequestId("x-1").toOption, Some("x-1"))
    assertEquals(parseConfigurationRequestId("invalid").toOption, None)
    assertEquals(parseConfigurationRequestId("a-1").toOption, None)
  }

  test("parseDatasetId") {
    assertEquals(parseDatasetId("d-1").toOption, Some("d-1"))
    assertEquals(parseDatasetId("invalid").toOption, None)
    assertEquals(parseDatasetId("a-1").toOption, None)
  }

  test("parseExecutionEventId") {
    assertEquals(parseExecutionEventId("e-1").toOption, Some("e-1"))
    assertEquals(parseExecutionEventId("invalid").toOption, None)
    assertEquals(parseExecutionEventId("a-1").toOption, None)
  }

  test("parseGroupId") {
    assertEquals(parseGroupId("g-1").toOption, Some("g-1"))
    assertEquals(parseGroupId("invalid").toOption, None)
    assertEquals(parseGroupId("a-1").toOption, None)
  }

  test("parseObservationId") {
    assertEquals(parseObservationId("o-1").toOption, Some("o-1"))
    assertEquals(parseObservationId("o-abc").toOption, Some("o-abc"))
    assertEquals(parseObservationId("invalid").toOption, None)
    assertEquals(parseObservationId("p-1").toOption, None)
  }

  test("parseProgramId") {
    assertEquals(parseProgramId("p-1").toOption, Some("p-1"))
    assertEquals(parseProgramId("p-ff").toOption, Some("p-ff"))
    assertEquals(parseProgramId("invalid").toOption, None)
    assertEquals(parseProgramId("o-1").toOption, None)
  }

  test("parseProgramNoteId") {
    assertEquals(parseProgramNoteId("n-1").toOption, Some("n-1"))
    assertEquals(parseProgramNoteId("invalid").toOption, None)
    assertEquals(parseProgramNoteId("a-1").toOption, None)
  }

  test("parseProgramUserId") {
    assertEquals(parseProgramUserId("m-1").toOption, Some("m-1"))
    assertEquals(parseProgramUserId("invalid").toOption, None)
    assertEquals(parseProgramUserId("a-1").toOption, None)
  }

  test("parseStandardRoleId") {
    assertEquals(parseStandardRoleId("r-1").toOption, Some("r-1"))
    assertEquals(parseStandardRoleId("invalid").toOption, None)
    assertEquals(parseStandardRoleId("a-1").toOption, None)
  }

  test("parseTargetId") {
    assertEquals(parseTargetId("t-1").toOption, Some("t-1"))
    assertEquals(parseTargetId("t-ff").toOption, Some("t-ff"))
    assertEquals(parseTargetId("invalid").toOption, None)
    assertEquals(parseTargetId("o-1").toOption, None)
  }

  test("parseUserId") {
    assertEquals(parseUserId("u-1").toOption, Some("u-1"))
    assertEquals(parseUserId("invalid").toOption, None)
    assertEquals(parseUserId("a-1").toOption, None)
  }

  test("parseVisitId") {
    assertEquals(parseVisitId("v-1").toOption, Some("v-1"))
    assertEquals(parseVisitId("invalid").toOption, None)
    assertEquals(parseVisitId("a-1").toOption, None)
  }

  test("parseDmsString") {
    // Canonical output format is DD:MM:SS.MMMMMM (no sign); input requires a sign
    assertEquals(parseDmsString("+00:00:00.000000").toOption, Some("00:00:00.000000"))
    assertEquals(parseDmsString("+15:00:00.00").toOption, Some("15:00:00.000000"))
    assertEquals(parseDmsString("-15:00:00.00").toOption, Some("345:00:00.000000"))
    assertEquals(parseDmsString("invalid").toOption, None)
    assertEquals(parseDmsString("000:00:00.000").toOption, None)
  }

  test("parseHmsString") {
    // Canonical output format is HH:MM:SS.MMMMMM
    assertEquals(parseHmsString("00:00:00.000").toOption, Some("00:00:00.000000"))
    assertEquals(parseHmsString("01:00:00.000").toOption, Some("01:00:00.000000"))
    assertEquals(parseHmsString("12:00:00.000").toOption, Some("12:00:00.000000"))
    assertEquals(parseHmsString("invalid").toOption, None)
  }

  test("parseEpochString") {
    // Only Julian scheme is supported; valid milliyear range is [1972000, 3000999]
    assertEquals(parseEpochString("J2000.000").toOption, Some("J2000.000"))
    assertEquals(parseEpochString("J2025.000").toOption, Some("J2025.000"))
    assertEquals(parseEpochString("B1950.000").toOption, None)
    assertEquals(parseEpochString("invalid").toOption, None)
  }

  test("toRightAscension") {
    // hms/HMS format: HH:MM:SS.MMMMMM (6 sub-second digits)
    val ra0 = toRightAscension(0.0)
    assertNoDiff(ra0.hms, "00:00:00.000000")
    assertEquals(ra0.degrees, 0.0)
    assertEquals(ra0.hours, 0.0)
    assertNoDiff(ra0.microseconds.toString(), "0")

    val ra180 = toRightAscension(180.0)
    assertNoDiff(ra180.hms, "12:00:00.000000")
    assertEquals(ra180.degrees, 180.0)
    assertEquals(ra180.hours, 12.0)
  }

  test("toDeclination") {
    // dms format: +/-DD:MM:SS.MMMMMM (6 sub-second digits, always signed)
    val dec0 = toDeclination(0.0)
    assertNoDiff(dec0.dms, "+00:00:00.000000")
    assertEquals(dec0.degrees, 0.0)
    assertNoDiff(dec0.microarcseconds.toString(), "0")

    val dec45 = toDeclination(45.0)
    assertNoDiff(dec45.dms, "+45:00:00.000000")
    assertEquals(dec45.degrees, 45.0)

    val decNeg = toDeclination(-30.0)
    assertNoDiff(decNeg.dms, "-30:00:00.000000")
    // Negative declinations are stored as angles in [270°, 360°), so -30° → 330°
    assertEquals(decNeg.degrees, 330.0)
  }

  test("toAngle") {
    // dms format: DD:MM:SS.MMMMMM (6 sub-second digits, no sign)
    val a0 = toAngle(0.0)
    assertNoDiff(a0.dms, "00:00:00.000000")
    assertEquals(a0.degrees, 0.0)
    assertEquals(a0.arcseconds, 0.0)

    val a90 = toAngle(90.0)
    assertNoDiff(a90.dms, "90:00:00.000000")
    assertEquals(a90.degrees, 90.0)
    assertEquals(a90.arcseconds, 324000.0)
  }

  test("toProperMotion") {
    val pm = toProperMotion(1000, -500)
    assertNoDiff(pm.ra.microarcsecondsPerYear.toString(), "1000")
    assertNoDiff(pm.dec.microarcsecondsPerYear.toString(), "-500")
    assertEquals(pm.ra.milliarcsecondsPerYear, 1.0)
    assertEquals(pm.dec.milliarcsecondsPerYear, -0.5)
  }
}

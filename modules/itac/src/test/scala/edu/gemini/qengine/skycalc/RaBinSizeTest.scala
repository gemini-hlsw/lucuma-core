// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import munit.FunSuite

class RaBinSizeTest extends FunSuite {

  test("testNegativeSize") {
    assert(RaBinSize.ofArcMinutes(-1).isEmpty)
  }

  test("testZeroSize") {
    assert(RaBinSize.ofArcMinutes(0).isEmpty)
  }

  test("testOddSize") {
    assert(RaBinSize.ofArcMinutes(37).isEmpty)
  }

  test("testBinCount") {
    val s30 = RaBinSize.ofArcMinutes(30).get
    assertEquals(48, s30.binCount)

    val s60 = RaBinSize.ofArcMinutes(60).get
    assertEquals(24, s60.binCount)

    val s120 = RaBinSize.ofArcMinutes(120).get
    assertEquals(12, s120.binCount)
  }

  test("testGenRas") {
    val s60   = RaBinSize.ofArcMinutes(60).get
    val ras60 = s60.genRas
    assertEquals(24, ras60.size)

    assertEquals((30 to 1410 by 60).map(_.toDouble).toList, ras60.map(_.toHourAngle.toDoubleMinutes))

    val s120   = RaBinSize.ofArcMinutes(120).get
    val ras120 = s120.genRas
    assertEquals(12, ras120.size)

    assertEquals((60 to 1380 by 120).map(_.toDouble).toList, ras120.map(_.toHourAngle.toDoubleMinutes))

  }

  test("testGenTargets") {
    val sz = DecBinSize.ofDegrees(20).get
    val ra = RightAscension(HourAngle.fromDoubleHours(12))
    val wc = sz.genCoordinates(ra)

    val expected =
      (-80 to 80 by 20).map(Declination.fromDoubleDegrees(_).get).map(Coordinates(ra, _))

    expected
      .zip(wc)
      .foreach(tup => {
        val exp = tup._1
        val act = tup._2
        assertEquals(RightAscension.fromDoubleDegrees(180.0), act.ra)
        assertEquals(exp.dec, act.dec)
      })
  }

}

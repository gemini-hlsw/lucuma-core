// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc
import munit.FunSuite

class DecBinSizeTest extends FunSuite {

  test("testNegativeSize") {
    assert(DecBinSize.ofDegrees(-1).isEmpty)
  }

  test("testZeroSize") {
    assert(DecBinSize.ofDegrees(0).isEmpty)
  }

  test("testOddSize") {
    assert(DecBinSize.ofDegrees(37).isEmpty)
  }

  test("testBinCount") {
    val s5 = DecBinSize.ofDegrees(5).get
    assertEquals(36, s5.binCount)

    val s10 = DecBinSize.ofDegrees(10).get
    assertEquals(18, s10.binCount)

    val s20 = DecBinSize.ofDegrees(20).get
    assertEquals(9, s20.binCount)
  }

  test("testGenDecs") {
    val s10   = DecBinSize.ofDegrees(10).get
    val dec10 = s10.genDecs
    assertEquals(18, dec10.size)

    assertEquals(dec10.map(_.toAngle.toSignedDoubleDegrees), (-85 to 85 by 10).map(_.toDouble).toList)

    val s20   = DecBinSize.ofDegrees(20).get
    val dec20 = s20.genDecs
    assertEquals(9, dec20.size)

    assertEquals(dec20.map(_.toAngle.toSignedDoubleDegrees), (-80 to 80 by 20).map(_.toDouble).toList)
  }

}

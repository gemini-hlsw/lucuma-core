// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Target
import lucuma.core.math.Declination
import lucuma.core.model.IntCentiPercent
import munit.FunSuite

class DecBinGroupTest extends FunSuite {

  private def f(decRange: DecRange): Option[IntCentiPercent] = {
    decRange.startDeg match {
      case neg if neg < 0 => None
      case pos => Some(IntCentiPercent.unsafeFromPercent(decRange.startDeg))
    }
  }

  test("testGen10deg") {
    val grp = DeclinationMap.gen10DegBins(f)
    assertEquals(None, grp.get(Declination.fromDoubleDegrees(-10).get))
    assertEquals(IntCentiPercent.unsafeFromPercent(0), grp.get(Declination.Zero).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(0), grp.get(Declination.fromDoubleDegrees(9.9).get).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(80), grp.get(Declination.fromDoubleDegrees(80).get).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(80), grp.get(Declination.fromDoubleDegrees(90).get).get.binValue)
  }

  test("testGen20deg") {
    val grp = DeclinationMap.gen20DegBins(f)
    assertEquals(None, grp.get(Declination.fromDoubleDegrees(-10).get))
    assertEquals(None, grp.get(Declination.Zero))
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp.get(Declination.fromDoubleDegrees(10).get).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp.get(Declination.fromDoubleDegrees(19.9).get).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(70), grp.get(Declination.fromDoubleDegrees(70).get).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(70), grp.get(Declination.fromDoubleDegrees(90).get).get.binValue)
  }

  test("testBadBinSize") {
    try {
      DeclinationMap.gen(17)(f)
      fail
    } catch {
      case ex: IllegalArgumentException => // ok
    }
  }

  test("testUpdatedFunSome") {
    val grp = DeclinationMap.gen10DegBins(f)
    val grp2 = grp.updated(Declination.Zero, p => Some(IntCentiPercent.unsafeFromPercent(p.toPercent + 1))).get
    assertEquals(IntCentiPercent.unsafeFromPercent(1), grp2.get(Declination.Zero).get.binValue)
  }

  test("testUpdatedFunNone") {
    val grp = DeclinationMap.gen10DegBins(f)
    val opt = grp.updated(Declination.fromDoubleDegrees(-10).get, p => Some(IntCentiPercent.unsafeFromPercent(p.toPercent + 1)))
    assertEquals(None, opt)
  }

  test("testUpdatedSome") {
    val grp = DeclinationMap.gen10DegBins(f)
    val grp2 = grp.updated(Declination.Zero, IntCentiPercent.unsafeFromPercent(42)).get
    assertEquals(IntCentiPercent.unsafeFromPercent(42), grp2.get(Declination.Zero).get.binValue)
  }

  test("testUpdatedNone") {
    val grp = DeclinationMap.gen10DegBins(f)
    assertEquals(None, grp.updated(Declination.fromDoubleDegrees(-10).get, IntCentiPercent.unsafeFromPercent(42)))
  }

  test("testIndexOf") {
    val grp = DeclinationMap.gen10DegBins(f)
    assertEquals(-1, grp.indexOf(Declination.fromDoubleDegrees(-10).get))
    assertEquals( 0, grp.indexOf(Declination.Zero))
    assertEquals( 8, grp.indexOf(Declination.fromDoubleDegrees( 80).get))
    assertEquals( 8, grp.indexOf(Declination.fromDoubleDegrees( 90).get))
  }

  test("testApplyTarget") {
    val grp = DeclinationMap.gen10DegBins(f)
    assertEquals(IntCentiPercent.unsafeFromPercent( 0), grp.get(Target(12 * 15, 0)).get.binValue)
    assertEquals(IntCentiPercent.unsafeFromPercent(70), grp.get(Target(12 * 16, 70)).get.binValue)
    assertEquals(None, grp.get(Target(12 * 15, -90)))
  }

  test("testGenFromBinValues") {
    val percs = List(IntCentiPercent.unsafeFromPercent(0), IntCentiPercent.unsafeFromPercent(25), IntCentiPercent.unsafeFromPercent(100), IntCentiPercent.unsafeFromPercent(50))
    val grp = DeclinationMap(percs)

    val expected = List(
      DecRanged(DecRange(-90, -45), IntCentiPercent.unsafeFromPercent(  0)),
      DecRanged(DecRange(-45,   0), IntCentiPercent.unsafeFromPercent( 25)),
      DecRanged(DecRange(  0,  45), IntCentiPercent.unsafeFromPercent(100)),
      DecRanged(DecRange( 45,  90).inclusive, IntCentiPercent.unsafeFromPercent(50))
    )

    assertEquals(expected, grp.bins.toList)
  }
}
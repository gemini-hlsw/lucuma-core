// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.util.Percent
import munit.FunSuite
import lucuma.core.math.Declination
import edu.gemini.tac.qengine.p1.Target

class DecBinGroupTest extends FunSuite {

  private def f(decRange: DecRange): Option[Percent] = {
    decRange.startDeg match {
      case neg if neg < 0 => None
      case pos => Some(Percent(decRange.startDeg))
    }
  }

  test("testGen10deg") {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(None, grp.get(Declination.fromDoubleDegrees(-10).get))
    assertEquals(Percent(0), grp.get(Declination.Zero).get.binValue)
    assertEquals(Percent(0), grp.get(Declination.fromDoubleDegrees(9.9).get).get.binValue)
    assertEquals(Percent(80), grp.get(Declination.fromDoubleDegrees(80).get).get.binValue)
    assertEquals(Percent(80), grp.get(Declination.fromDoubleDegrees(90).get).get.binValue)
  }

  test("testGen20deg") {
    val grp = DecBinGroup.gen20DegBins(f)
    assertEquals(None, grp.get(Declination.fromDoubleDegrees(-10).get))
    assertEquals(None, grp.get(Declination.Zero))
    assertEquals(Percent(10), grp.get(Declination.fromDoubleDegrees(10).get).get.binValue)
    assertEquals(Percent(10), grp.get(Declination.fromDoubleDegrees(19.9).get).get.binValue)
    assertEquals(Percent(70), grp.get(Declination.fromDoubleDegrees(70).get).get.binValue)
    assertEquals(Percent(70), grp.get(Declination.fromDoubleDegrees(90).get).get.binValue)
  }

  test("testBadBinSize") {
    try {
      DecBinGroup.gen(17)(f)
      fail
    } catch {
      case ex: IllegalArgumentException => // ok
    }
  }

  test("testUpdatedFunSome") {
    val grp = DecBinGroup.gen10DegBins(f)
    val grp2 = grp.updated(Declination.Zero, p => Some(Percent(p.value + 1))).get
    assertEquals(Percent(1), grp2.get(Declination.Zero).get.binValue)
  }

  test("testUpdatedFunNone") {
    val grp = DecBinGroup.gen10DegBins(f)
    val opt = grp.updated(Declination.fromDoubleDegrees(-10).get, p => Some(Percent(p.value + 1)))
    assertEquals(None, opt)
  }

  test("testUpdatedSome") {
    val grp = DecBinGroup.gen10DegBins(f)
    val grp2 = grp.updated(Declination.Zero, Percent(42)).get
    assertEquals(Percent(42), grp2.get(Declination.Zero).get.binValue)
  }

  test("testUpdatedNone") {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(None, grp.updated(Declination.fromDoubleDegrees(-10).get, Percent(42)))
  }

  test("testIndexOf") {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(-1, grp.indexOf(Declination.fromDoubleDegrees(-10).get))
    assertEquals( 0, grp.indexOf(Declination.Zero))
    assertEquals( 8, grp.indexOf(Declination.fromDoubleDegrees( 80).get))
    assertEquals( 8, grp.indexOf(Declination.fromDoubleDegrees( 90).get))
  }

  test("testApplyTarget") {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(Percent( 0), grp.get(Target(12 * 15, 0)).get.binValue)
    assertEquals(Percent(70), grp.get(Target(12 * 16, 70)).get.binValue)
    assertEquals(None, grp.get(Target(12 * 15, -90)))
  }

  test("testGenFromBinValues") {
    val percs = List(Percent(0), Percent(25), Percent(100), Percent(50))
    val grp = DecBinGroup(percs)

    val expected = List(
      DecBin(DecRange(-90, -45), Percent(  0)),
      DecBin(DecRange(-45,   0), Percent( 25)),
      DecBin(DecRange(  0,  45), Percent(100)),
      DecBin(DecRange( 45,  90).inclusive, Percent(50))
    )

    assertEquals(expected, grp.bins.toList)
  }
}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import org.junit._
import Assert._

import scala.jdk.CollectionConverters._
import edu.gemini.skycalc.Angle
import jsky.coords.WorldCoords

class RaBinSizeTest {
  @Test def testNegativeSize() {
    try {
      new RaBinSize(-1)
    } catch {
      case ex: BinSizeException => {
        assertEquals(-1, ex.getBadSize)
        assertTrue(ex.getMessage.contains("negative"))
      }
    }
  }

  @Test def testZeroSize() {
    try {
      new RaBinSize(0)
    } catch {
      case ex: BinSizeException => {
        assertEquals(0, ex.getBadSize)
        assertTrue(ex.getMessage.contains("evenly divide"))
      }
    }
  }

  @Test def testOddSize() {
    try {
      new RaBinSize(37)
    } catch {
      case ex: BinSizeException => {
        assertEquals(37, ex.getBadSize)
        assertTrue(ex.getMessage.contains("evenly divide"))
      }
    }
  }

  @Test def testBinCount() {
    val s30 = new RaBinSize(30)
    assertEquals(48, s30.getBinCount)

    val s60 = new RaBinSize(60)
    assertEquals(24, s60.getBinCount)

    val s120 = new RaBinSize(120)
    assertEquals(12, s120.getBinCount)
  }

  @Test def testGenRas() {
    val s60   = new RaBinSize(60)
    val ras60 = s60.genRas.asScala.toList
    assertEquals(24, ras60.size)

    assertEquals(30 to 1410 by 60, ras60.map(_.toMinutes.getMagnitude.toInt))

    val s120   = new RaBinSize(120)
    val ras120 = s120.genRas.asScala.toList
    assertEquals(12, ras120.size)

    assertEquals(60 to 1380 by 120, ras120.map(_.toMinutes.getMagnitude.toInt))
  }

  @Test def testGenTargets() {
    val sz = new DecBinSize(20)
    val ra = new Angle(12, Angle.Unit.HOURS)
    val wc = sz.genTargets(ra).asScala.toList

    val expected =
      (-80 to 80 by 20).map(dec => new WorldCoords(ra.toDegrees.getMagnitude, dec.toDouble))

    expected
      .zip(wc)
      .foreach(tup => {
        val exp = tup._1
        val act = tup._2
        assertEquals(180.0, act.getRaDeg, 0.000001)
        assertEquals(exp.getDecDeg, act.getDecDeg, 0.000001)
      })
  }
}

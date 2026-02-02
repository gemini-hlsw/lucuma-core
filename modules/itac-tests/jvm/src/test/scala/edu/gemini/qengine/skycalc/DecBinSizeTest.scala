// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import org.junit._
import Assert._

import scala.jdk.CollectionConverters._

class DecBinSizeTest {
  @Test def testNegativeSize() = {
    try {
      new DecBinSize(-1)
    } catch {
      case ex: BinSizeException => {
        assertEquals(-1, ex.getBadSize)
        assertTrue(ex.getMessage.contains("negative"))
      }
    }
  }

  @Test def testZeroSize() = {
    try {
      new DecBinSize(0)
    } catch {
      case ex: BinSizeException => {
        assertEquals(0, ex.getBadSize)
        assertTrue(ex.getMessage.contains("evenly divide"))
      }
    }
  }
  @Test def testOddSize() = {
    try {
      new DecBinSize(37)
    } catch {
      case ex: BinSizeException => {
        assertEquals(37, ex.getBadSize)
        assertTrue(ex.getMessage.contains("evenly divide"))
      }
    }
  }

  @Test def testBinCount() = {
    val s5 = new DecBinSize(5)
    assertEquals(36, s5.getBinCount)

    val s10 = new DecBinSize(10)
    assertEquals(18, s10.getBinCount)

    val s20 = new DecBinSize(20)
    assertEquals(9, s20.getBinCount)
  }

  @Test def testGenDecs() = {
    val s10   = new DecBinSize(10)
    val dec10 = s10.genDecs.asScala.toList
    assertEquals(18, dec10.size)

    assertEquals(-85 to 85 by 10, dec10.map(_.toDegrees.getMagnitude.toInt))

    val s20   = new DecBinSize(20)
    val dec20 = s20.genDecs.asScala.toList
    assertEquals(9, dec20.size)

    assertEquals(-80 to 80 by 20, dec20.map(_.toDegrees.getMagnitude.toInt))
  }
}

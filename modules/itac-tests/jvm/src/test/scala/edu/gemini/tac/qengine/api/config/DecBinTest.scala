// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.Percent
import org.junit.*

import Assert.*

class DecBinTest {
  val TenPercent = Percent(10)

  val bin = DecBin(0, 10, TenPercent)
  val ibin = DecBin.inclusive(0, 10, TenPercent)

  @Test def testInclusive() = {
    assertTrue(bin.inclusive.range.isInclusive)
    assertTrue(bin.inclusive.range.contains(Target(0, 10.0)))
    assertSame(ibin, ibin.inclusive)
  }

  @Test def testEquals() = {
    assertEquals(bin, DecBin(0, 10, TenPercent))
    assertFalse(bin.equals(ibin))
    assertFalse(bin.equals(DecBin(1, 10, TenPercent)))
    assertFalse(bin.equals(DecBin(0,  9, TenPercent)))
    assertFalse(bin.equals(DecBin(0, 10, Percent(11))))
  }

  @Test def testMap() = {
    assertEquals(10, DecBin(10, 20, TenPercent).map(_.value).binValue.doubleValue, Double.MinPositiveValue)
  }
}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.Percent
import munit.FunSuite

class DecBinTest extends FunSuite {
  val TenPercent = Percent(10)

  val bin = DecBin(0, 10, TenPercent)
  val ibin = DecBin.inclusive(0, 10, TenPercent)

  test("testInclusive") {
    assert(bin.inclusive.range.isInclusive)
    assert(bin.inclusive.range.contains(Target(0, 10.0)))
    assert(ibin eq ibin.inclusive)
  }

  test("testEquals") {
    assertEquals(bin, DecBin(0, 10, TenPercent))
    assert(!bin.equals(ibin))
    assert(!bin.equals(DecBin(1, 10, TenPercent)))
    assert(!bin.equals(DecBin(0,  9, TenPercent)))
    assert(!bin.equals(DecBin(0, 10, Percent(11))))
  }

  test("testMap") {
    assertEquals(10.0, DecBin(10, 20, TenPercent).map(_.value).binValue.doubleValue, Double.MinPositiveValue)
  }
}
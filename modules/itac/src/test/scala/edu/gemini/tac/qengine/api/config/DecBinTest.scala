// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.ItacSuite
import edu.gemini.tac.qengine.p1.ItacTarget
import lucuma.core.model.IntCentiPercent

class DecBinTest extends ItacSuite {
  val TenPercent = IntCentiPercent.unsafeFromPercent(10)

  val bin = DecRanged(0, 10, TenPercent)
  val ibin = DecRanged.inclusive(0, 10, TenPercent)

  test("testInclusive") {
    assert(bin.inclusive.range.isInclusive)
    assert(bin.inclusive.range.contains(ItacTarget(0, 10.0)))
    assert(ibin eq ibin.inclusive)
  }

  test("testEquals") {
    assertEquals(bin, DecRanged(0, 10, TenPercent))
    assert(!bin.equals(ibin))
    assert(!bin.equals(DecRanged(1, 10, TenPercent)))
    assert(!bin.equals(DecRanged(0,  9, TenPercent)))
    assert(!bin.equals(DecRanged(0, 10, IntCentiPercent.unsafeFromPercent(11))))
  }

  test("testMap") {
    assertEquals(10.0, DecRanged(10, 20, TenPercent).map(_.toPercent).binValue.doubleValue, Double.MinPositiveValue)
  }
}
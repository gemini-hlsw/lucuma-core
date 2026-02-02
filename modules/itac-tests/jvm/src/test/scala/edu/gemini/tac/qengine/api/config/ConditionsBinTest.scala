// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.api.config.ConditionsCategory.Eq
import edu.gemini.tac.qengine.p1.CloudCover.CC50
import edu.gemini.tac.qengine.p1.CloudCover.CC70
import edu.gemini.tac.qengine.util.Percent
import org.junit.*

import Assert.*

class ConditionsBinTest {
  private val bin = ConditionsBin(ConditionsCategory(), Percent(10))

  @Test def testMap() = {
    assertEquals(10, bin.map(_.value).binValue.doubleValue, Double.MinPositiveValue)
  }

  @Test def testUpdated() = {
    val up = bin.updated(Percent(20))
    assertEquals(Percent(20), up.binValue)
    assertEquals(bin.cat, up.cat)
  }

  @Test def testList() = {
    val cat0 = ConditionsCategory(Eq(CC50))
    val cat1 = ConditionsCategory(Eq(CC70))

    val l = ConditionsBin.of((cat0, Percent(10)), (cat1, Percent(20)))
    assertEquals(10, l.head.binValue.doubleValue, Double.MinPositiveValue)
    assertEquals(20, l.last.binValue.doubleValue, Double.MinPositiveValue)
    assertEquals(cat0, l.head.cat)
    assertEquals(cat1, l.last.cat)
  }
}
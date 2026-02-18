// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.api.config.ConditionsCategory.Eq
import edu.gemini.tac.qengine.util.Percent
import munit.FunSuite
import lucuma.core.model.CloudExtinction
import cats.implicits.*

class ConditionsBinTest extends FunSuite {
  private val bin = ConditionsBin(ConditionsCategory(), Percent(10))

  test("testMap") {
    assertEquals(10.0, bin.map(_.value).binValue.doubleValue, Double.MinPositiveValue)
  }

  test("testUpdated") {
    val up = bin.updated(Percent(20))
    assertEquals(Percent(20), up.binValue)
    assertEquals(bin.cat, up.cat)
  }

  test("testList") {
    val cat0 = ConditionsCategory(Eq(CloudExtinction.Preset.Zero))
    val cat1 = ConditionsCategory(Eq(CloudExtinction.Preset.PointThree))

    val l = ConditionsBin.of((cat0, Percent(10)), (cat1, Percent(20)))
    assertEquals(10.0, l.head.binValue.doubleValue, Double.MinPositiveValue)
    assertEquals(20.0, l.last.binValue.doubleValue, Double.MinPositiveValue)
    assertEquals(cat0, l.head.cat)
    assertEquals(cat1, l.last.cat)
  }
}
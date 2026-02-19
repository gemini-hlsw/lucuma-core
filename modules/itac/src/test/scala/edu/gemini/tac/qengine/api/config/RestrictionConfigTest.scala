// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import cats.syntax.all.*
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.WaterVapor
import lucuma.core.model.IntCentiPercent
import munit.FunSuite

class RestrictionConfigTest extends FunSuite {
  test("testMapCombine") {
    val percentBin = TimeRestriction("WV", IntCentiPercent.unsafeFromPercent(10)) {
      (_, obs, _) => obs.conditions.waterVapor <= WaterVapor.Dry
    }
    val timeBin = TimeRestriction("LGS", Time.hours(10)) {
      (_, obs, _) => obs.lgs
    }

    val conf = new RestrictionConfig(List(percentBin), List(timeBin))
    val comb = conf.mapTimeRestrictions(_.toPercent.toLong, _.ms)

    assertEquals(2, comb.length)
    assertEquals(10l, comb.head.value)
    assertEquals(10 * 60 * 60 * 1000l, comb.tail.head.value)
  }
}
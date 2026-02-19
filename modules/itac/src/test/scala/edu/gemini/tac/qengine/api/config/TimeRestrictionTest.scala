// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.SkyBackground.SBAny
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import munit.FunSuite

import scala.Ordering.Implicits.*

class TimeRestrictionTest extends FunSuite {

  val US = TimeAccountingCategory.US

  private val ntac   = Ntac(US, "x", 0, Time.hours(10))
  private val target = Target(0.0, 0.0) // not used
  private def conds(wv: WaterVapor) =
    ObservingConditions(CloudExtinction.Preset.ThreePointZero, ImageQuality.Preset.TwoPointZero, SBAny, wv)

  private val bin = TimeRestriction("wv", Percent(10)) {
    (_, obs, _) => obs.conditions.wv <= WV50
  }

  private def mkProp(wv: WaterVapor): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(Observation(target, conds(wv), Time.hours(10))))


  test("testMatches") {
    val propList = WaterVapor.values.map(mkProp(_))
    val boolList = propList.map(prop => bin.matches(prop, prop.obsList.head, ScienceBand.Band1))
    assertEquals(List(true, true, false, false), boolList)
  }

  test("testUpdated") {
    assertEquals(Percent(20), bin.updated(Percent(20)).value)
  }

  test("testMap") {
    assertEquals(Time.hours(10), bin.map(perc => Time.hours(100) * perc).value)
  }
}
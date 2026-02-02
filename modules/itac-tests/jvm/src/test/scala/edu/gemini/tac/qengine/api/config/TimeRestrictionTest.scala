// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.CloudCover.CCAny
import edu.gemini.tac.qengine.p1.ImageQuality.IQAny
import edu.gemini.tac.qengine.p1.SkyBackground.SBAny
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site
import org.junit.*

import scala.Ordering.Implicits.*

import Assert.*

class TimeRestrictionTest {

  val US = Partner.US

  private val ntac   = Ntac(US, "x", 0, Time.hours(10))
  private val target = Target(0.0, 0.0) // not used
  private def conds(wv: WaterVapor) =
    ObservingConditions(CCAny, IQAny, SBAny, wv)

  private val bin = TimeRestriction("wv", Percent(10)) {
    (_, obs, _) => obs.conditions.wv <= WV50
  }

  private def mkProp(wv: WaterVapor): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(Observation(null, target, conds(wv), Time.hours(10))))


  @Test def testMatches() = {
    val propList = WaterVapor.values.map(mkProp(_))
    val boolList = propList.map(prop => bin.matches(prop, prop.obsList.head, QueueBand.QBand1))
    assertEquals(List(true, true, false, false), boolList)
  }

  @Test def testUpdated() = {
    assertEquals(Percent(20), bin.updated(Percent(20)).value)
  }

  @Test def testMap() = {
    assertEquals(Time.hours(10), bin.map(perc => Time.hours(100) * perc).value)
  }
}
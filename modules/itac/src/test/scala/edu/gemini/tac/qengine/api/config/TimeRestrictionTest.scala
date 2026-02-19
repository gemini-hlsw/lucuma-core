// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import cats.syntax.all.*
import edu.gemini.tac.qengine.ItacSuite
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.IntCentiPercent
import lucuma.core.util.Enumerated

class TimeRestrictionTest extends ItacSuite {

  val US = TimeAccountingCategory.US

  private val ntac   = Ntac(US, "x", 0, Time.hours(10))
  private val target = ItacTarget(0.0, 0.0) // not used
  private def conds(wv: WaterVapor) =
    ConstraintSet(ImageQuality.Preset.TwoPointZero, CloudExtinction.Preset.ThreePointZero, SkyBackground.Bright, wv, ElevationRange.ByAirMass.Default)

  private val bin = TimeRestriction("wv", IntCentiPercent.unsafeFromPercent(10)) {
    (_, obs, _) => obs.constraintSet.waterVapor <= WaterVapor.Dry
  }

  private def mkProp(wv: WaterVapor): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(ItacObservation(target, conds(wv), Time.hours(10))))


  test("testMatches") {
    val propList = Enumerated[WaterVapor].all.map(mkProp(_))
    val boolList = propList.map(prop => bin.matches(prop, prop.obsList.head, ScienceBand.Band1))
    assertEquals(List(true, true, false, false), boolList)
  }

  test("testUpdated") {
    assertEquals(IntCentiPercent.unsafeFromPercent(20), bin.updated(IntCentiPercent.unsafeFromPercent(20)).value)
  }

  test("testMap") {
    assertEquals(Time.hours(10), bin.map(perc => Time.hours(100) * perc).value)
  }
}
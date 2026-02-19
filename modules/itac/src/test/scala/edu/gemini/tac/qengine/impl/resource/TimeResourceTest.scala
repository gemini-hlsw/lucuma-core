// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import cats.syntax.all.*
import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectRestrictedBin
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.util.Enumerated
import munit.FunSuite
import org.junit.*

import Assert.*

class TimeRestrictionResourceTest extends FunSuite {
  import TimeAccountingCategory.US
  val TimeAccountingCategorys = Enumerated[TimeAccountingCategory].all

  private val ntac   = Ntac(US, "x", 0, Time.hours(10))
  private val target = Target(Coordinates.Zero) // not used
  private def conds(wv: WaterVapor) =
    ObservingConditions(CloudExtinction.Preset.ThreePointZero, ImageQuality.Preset.TwoPointZero, SkyBackground.Bright, wv)

  private val bin = TimeRestriction("WV", Percent(10)) {
    (_, obs, _) => obs.conditions.wv <= WaterVapor.Dry
  }

  // 10% of 10 hours = 1 hr = 60 min
  private val res60min = TimeRestrictionResource(bin, Time.hours(10))

  private def mkProp(wv: WaterVapor): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(Observation(target, conds(wv), Time.hours(10))))

  test("testReserveNoMatch") {
    val prop = mkProp(WaterVapor.Median)

    // If the restriction doesn't match the block, then the same instance is
    // returned -- not a copy with the same values
    val block = Block(prop, prop.obsList.head, Time.hours(1))
    res60min.reserve(block, Fixture.emptyQueue) match {
      case Right(res) => assertSame(res60min, res)
      case _ => fail("failed")
    }
  }

  test("testReserveNoTime") {
    val prop = mkProp(WaterVapor.VeryDry)

    // Here the restriction matches the block, but we're not reserving any
    // time.  Again, no copy should be made
    val block = Block(prop, prop.obsList.head, Time.hours(0))
    res60min.reserve(block, Fixture.emptyQueue) match {
      case Right(res) => assertSame(res60min, res)
      case _ => fail("failed")
    }
  }

  test("testReserve") {
    val prop = mkProp(WaterVapor.VeryDry)

    // Reserve 15 of the 60 available minutes
    val block = Block(prop, prop.obsList.head, Time.minutes(15))
    res60min.reserve(block, Fixture.emptyQueue) match {
      case Right(res) => assertEquals(Time.minutes(45), res.remaining)
      case _ => fail("failed")
    }
  }

  test("testReject") {
    val prop = mkProp(WaterVapor.VeryDry)

    // Try to reserve more than 1 hour
    val block = Block(prop, prop.obsList.head, Time.minutes(61))
    res60min.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectRestrictedBin) => assertEquals(prop, msg.prop)
      case _ => fail("failed")
    }
  }
}
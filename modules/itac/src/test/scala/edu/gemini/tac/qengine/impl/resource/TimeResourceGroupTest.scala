// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import cats.syntax.all.*
import edu.gemini.tac.qengine.ItacSuite
import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectRestrictedBin
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Time
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

class CompositeTimeRestrictionResourceTest extends ItacSuite {
  import TimeAccountingCategory.US
  val TimeAccountingCategorys = Enumerated[TimeAccountingCategory].all

  private val ntac   = Ntac(US, "x", 0, Time.hours(10))
  private val target = ItacTarget(0, 0) // not used
  private def conds(wv: WaterVapor) =
    ConstraintSet(ImageQuality.Preset.TwoPointZero, CloudExtinction.Preset.ThreePointZero, SkyBackground.Bright, wv, ElevationRange.ByAirMass.Default)

  private val wvBin  = TimeRestriction("WV", IntCentiPercent.unsafeFromPercent(10)) {
    (_, obs, _) => obs.constraintSet.waterVapor <= WaterVapor.Dry
  }

  private val lgsBin = TimeRestriction("lgs", Time.hours(1)) {
    (_, obs, _) => obs.lgs
  }

  // 10% of 10 hours = 1 hr = 60 min
  private val resWV60min  = TimeRestrictionResource(wvBin, Time.hours(10))
  private val resLgs60min = TimeRestrictionResource(lgsBin)

  private val lst = List(resWV60min, resLgs60min)
  private val grp = new CompositeTimeRestrictionResource(lst)

  private def mkProp(wv: WaterVapor, lgs: Boolean): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(ItacObservation(target, conds(wv), Time.hours(10), lgs)))

  test("testReserveWv") {
    val prop  = mkProp(WaterVapor.VeryDry, lgs = false)  // matches WV limit, not LGS limit
    val block = Block(prop, prop.obsList.head, Time.minutes(15))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Right(newGrp) => {
        val res1 = newGrp.lst.head
        val res2 = newGrp.lst.tail.head

        assertEquals(Time.minutes(45), res1.remaining)
        assertEquals(Time.minutes(60), res2.remaining)
      }
      case _ => fail("failed")
    }
  }

  test("testReserveLgs") {
    val prop  = mkProp(WaterVapor.Median, lgs = true) // matches LGS limit, not WV limit
    val block = Block(prop, prop.obsList.head, Time.minutes(15))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Right(newGrp) => {
        val res1 = newGrp.lst.head
        val res2 = newGrp.lst.tail.head

        assertEquals(Time.minutes(60), res1.remaining)
        assertEquals(Time.minutes(45), res2.remaining)
      }
      case _ => fail("failed")
    }
  }

  test("testReserveBoth") {
    val prop  = mkProp(WaterVapor.VeryDry, lgs = true) // matches WV and LGS
    val block = Block(prop, prop.obsList.head, Time.minutes(15))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Right(newGrp) => {
        val res1 = newGrp.lst.head
        val res2 = newGrp.lst.tail.head

        assertEquals(Time.minutes(45), res1.remaining)
        assertEquals(Time.minutes(45), res2.remaining)
      }
      case _ => fail("failed")
    }
  }

  test("testFailWv") {
    val prop  = mkProp(WaterVapor.VeryDry, lgs = false)  // matches WV limit, not LGS limit
    val block = Block(prop, prop.obsList.head, Time.minutes(61))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectRestrictedBin) => // ok
      case _ => fail("failed")
    }
  }

  test("testFailLgs") {
    val prop  = mkProp(WaterVapor.Median, lgs = true)  // matches LGS, not WV
    val block = Block(prop, prop.obsList.head, Time.minutes(61))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectRestrictedBin) => // ok
      case _ => fail("failed")
    }
  }

  test("testNoMatch") {
    val prop  = mkProp(WaterVapor.Median, lgs = false)  // matches LGS, not WV

    // no match so it doesn't matter that we try to reserve too much
    val block = Block(prop, prop.obsList.head, Time.minutes(61))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Right(newGrp) => {
        val res1 = newGrp.lst.head
        val res2 = newGrp.lst.tail.head

        assertEquals(Time.minutes(60), res1.remaining)
        assertEquals(Time.minutes(60), res2.remaining)
      }
      case _ => fail("failed")
    }
  }
}
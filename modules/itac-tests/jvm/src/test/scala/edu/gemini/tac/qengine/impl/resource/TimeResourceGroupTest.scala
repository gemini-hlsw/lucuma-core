// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.TimeRestriction
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectRestrictedBin
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.CloudCover.CCAny
import edu.gemini.tac.qengine.p1.ImageQuality.IQAny
import edu.gemini.tac.qengine.p1.SkyBackground.SBAny
import edu.gemini.tac.qengine.p1.WaterVapor
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import munit.FunSuite

import scala.Ordering.Implicits.*

class TimeResourceGroupTest extends FunSuite {
  import Partner.US
  val partners = Enumerated[Partner].all

  private val ntac   = Ntac(US, "x", 0, Time.hours(10))
  private val target = Target(0.0, 0.0) // not used
  private def conds(wv: WaterVapor) =
    ObservingConditions(CCAny, IQAny, SBAny, wv)

  private val wvBin  = TimeRestriction("WV", Percent(10)) {
    (_, obs, _) => obs.conditions.wv <= WV50
  }

  private val lgsBin = TimeRestriction("lgs", Time.hours(1)) {
    (_, obs, _) => obs.lgs
  }

  // 10% of 10 hours = 1 hr = 60 min
  private val resWV60min  = TimeResource(wvBin, Time.hours(10))
  private val resLgs60min = TimeResource(lgsBin)

  private val lst = List(resWV60min, resLgs60min)
  private val grp = new TimeResourceGroup(lst)

  private def mkProp(wv: WaterVapor, lgs: Boolean): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(Observation(null, target, conds(wv), Time.hours(10), lgs)))

  test("testReserveWv") {
    val prop  = mkProp(WV20, lgs = false)  // matches WV limit, not LGS limit
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
    val prop  = mkProp(WV80, lgs = true) // matches LGS limit, not WV limit
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
    val prop  = mkProp(WV20, lgs = true) // matches WV and LGS
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
    val prop  = mkProp(WV20, lgs = false)  // matches WV limit, not LGS limit
    val block = Block(prop, prop.obsList.head, Time.minutes(61))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectRestrictedBin) => // ok
      case _ => fail("failed")
    }
  }

  test("testFailLgs") {
    val prop  = mkProp(WV80, lgs = true)  // matches LGS, not WV
    val block = Block(prop, prop.obsList.head, Time.minutes(61))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectRestrictedBin) => // ok
      case _ => fail("failed")
    }
  }

  test("testNoMatch") {
    val prop  = mkProp(WV80, lgs = false)  // matches LGS, not WV

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
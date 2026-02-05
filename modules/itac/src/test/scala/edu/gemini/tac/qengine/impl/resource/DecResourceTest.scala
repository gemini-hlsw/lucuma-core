// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.DecBin
import edu.gemini.tac.qengine.api.config.DecBinGroup
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectTarget
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import munit.FunSuite
import org.junit.*

import Assert.*

class DecResourceTest extends FunSuite {
  import Partner.KR
  val partners = Enumerated[Partner].all

  private val bin1 = DecBin( 0, 10, BoundedTime(Time.hours(10)))
  private val bin2 = DecBin(10, 20, BoundedTime(Time.hours(20)))
  private val binGrp = DecBinGroup.fromBins(bin1, bin2)
  private val grp    = new DecResourceGroup(binGrp)

  private def target(dec: Double): Target = Target(0.0, dec)

  private val targetNeg = target(-1)    // before bin1
  private val target0   = target( 0)    // start of bin1
  private val target9   = target( 9.99) // within bin1
  private val target10  = target(10)    // start of bin2
//  private val target19  = target(19.99) // within bin2
  private val target20  = target(20)    // after bin2

  private val conds = ObservingConditions.AnyConditions
  private val ntac = Ntac(KR, "x", 0, Time.Zero)

  private def mkProp(target: Target): Proposal =
    Proposal(ntac, site = Site.GS, obsList = List(Observation(null, target, conds, Time.Zero)))

  test("testNormalReserveWithRemainingTime") {
    val prop   = mkProp(target0)
    val block  = Block(prop, prop.obsList.head, Time.hours(5))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg)   => fail("failed")
      case Right(grp2) =>
        assertEquals(Time.hours(5),  grp2.remaining(target0))   // 5 hours left in bin1
        assertEquals(Time.hours(5),  grp2.remaining(target9))   // same 5 hours left in bin1
        assertEquals(Time.hours(20), grp2.remaining(target10))  // all 20 hours left in bin2, nothing was put there
        assertEquals(Time.Zero,      grp2.remaining(targetNeg)) // there is no bin for this, so there is no time
        assertEquals(Time.Zero,      grp2.remaining(target20))  // ditto
    }
  }

  test("testNormalReserveAvailableWithRemainingTime") {
    val (grp2, rem) = grp.reserveAvailable(Time.hours(5), target0)
    assertEquals(Time.Zero, rem)
    assertFalse(grp2.isFull(target0))
    assertEquals(Time.hours(5),  grp2.remaining(target0))
    assertEquals(Time.hours(20), grp2.remaining(target10))  // all 20 hours left in bin2, nothing was put there
  }

  test("testReserveExactlyAllAvailableTime") {
    val prop   = mkProp(target0)
    val block  = Block(prop, prop.obsList.head, Time.hours(10))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg)   => fail("failed")
      case Right(grp2) =>
        assertEquals(Time.Zero,      grp2.remaining(target0))   // took all the time in bin1
        assertEquals(Time.Zero,      grp2.remaining(target9))   // ditto
        assertEquals(Time.hours(20), grp2.remaining(target10))  // all 20 hours left in bin2, nothing was put there
        assertEquals(Time.Zero,      grp2.remaining(targetNeg)) // there is no bin for this, so there is no time
        assertEquals(Time.Zero,      grp2.remaining(target20))  // ditto
    }
  }

  test("testReserveAvailableAllTime") {
    val (grp2, rem) = grp.reserveAvailable(Time.hours(10), target0)
    assertEquals(Time.Zero, rem)
    assertTrue(grp2.isFull(target0))
    assertEquals(Time.hours(0),  grp2.remaining(target0))
    assertEquals(Time.hours(20), grp2.remaining(target10))  // all 20 hours left in bin2, nothing was put there
  }

  test("testCannotReserveMoreTimeThanAvailable") {
    val prop   = mkProp(target10)
    val block  = Block(prop, prop.obsList.head, Time.hours(20.01))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectTarget) => assertEquals(prop, msg.prop)
      case _ => fail("failed")
    }
  }

  test("testReserveAvailableWhenRequestingMoreTimeThanAvailable") {
    val (grp2, rem) = grp.reserveAvailable(Time.hours(50), target0)
    assertEquals(Time.hours(40), rem)
    assertTrue(grp2.isFull(target0))
    assertEquals(Time.hours(0),  grp2.remaining(target0))
    assertEquals(Time.hours(20), grp2.remaining(target10))  // all 20 hours left in bin2, nothing was put there
  }

  test("testReserveToo") {
    val prop   = mkProp(target0).copy(too = Too.standard)
    val block  = Block(prop, prop.obsList.head, Time.hours(10))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg) => fail(msg.toString)
      case Right(grp2) => {
        assertEquals(Time.hours( 5), grp2.remaining(target0))  // half (5hrs) in first bin of 10 hours
        assertEquals(Time.hours(15), grp2.remaining(target10)) // half (5hrs) in second bin of 20 hours
      }
    }
  }

  // Spread over the two bins, but the first bin cannot handle an equal share.
  test("testReserveTooUnequal") {
    val prop   = mkProp(target0).copy(too = Too.standard)
    val block  = Block(prop, prop.obsList.head, Time.hours(22))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg) => fail(msg.toString)
      case Right(grp2) =>
        assertEquals(Time.hours(0), grp2.remaining(target0))  // 10 hrs (the max) in first bin of 10 hours
        assertEquals(Time.hours(8), grp2.remaining(target10)) // remainder (12 hrs) in the second bin of 20 hours
    }
  }

  test("testReserveTooOverallocate") {
    val prop   = mkProp(target0).copy(too = Too.standard)
    val block  = Block(prop, prop.obsList.head, Time.hours(30.001))

    grp.reserve(block, Fixture.emptyQueue) match {
      case Left(msg: RejectTarget) => assertEquals(prop, msg.prop)
      case _ => fail("failed")
    }
  }

}
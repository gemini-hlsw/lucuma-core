// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectConditions
import edu.gemini.tac.qengine.log.RejectTarget
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Time
import lucuma.core.util.Enumerated
import org.junit.*
import org.junit.Assert.*

import Fixture.{badCC, emptyQueue, goodCC}

class RaResourceTest {
  import Partner.KR
  val partners = Enumerated[Partner].all

  // Proposal partner, proposal time, and observation time are not used in this
  // test.  We will make time blocks with the explicit amount of time we need
  // for testing.

  private val ntac = Ntac(KR, "x", 0, Time.Zero)

  private def mkProp(target: Target, conds: ObservingConditions): Proposal =
    Fixture.mkProp(ntac,  (target, conds, Time.Zero))

  private def verifyReserve(raRes: RaResource) = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg

    // Took 15 minutes of the total of 1 hour for RA 1 hr at dec 0-45
    assertEquals(Time.minutes(45), raRes.remaining(target))
    // Took 15 minutes of the total of 30 for the Good CC conditions.
    assertEquals(Time.minutes(15), raRes.remaining(goodCC))

    // Remaining for this target and conditions is therefore 15 mins.
    assertEquals(Time.minutes(15), raRes.remaining(target, goodCC))

    // Didn't touch the time for the upper half of the Dec Range.  Should
    // still have all 30 minutes left.
    assertEquals(Time.minutes(30), raRes.remaining(Target(15.0, 45.0)))

    // The poor CC conditions have 30 + the 15 remaining for the good CC.
    assertEquals(Time.minutes(45), raRes.remaining(badCC))

    // As a whole, 45 minutes are remaining
    assertEquals(Time.minutes(45), raRes.remaining)
  }

  @Test def testReserve() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg
    val prop   = mkProp(target, goodCC)
    val block  = Block(prop, prop.obsList.head, Time.minutes(15))

    Fixture.raResGroup.grp(target).reserve(block, emptyQueue) match {
      case Left(msg)     => fail(msg.reason)
      case Right(newRes) => verifyReserve(newRes)
    }
  }

  @Test def testReserveAvailable() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg
    val raGrp  = Fixture.raResGroup.grp(target)
    val (newRes, rem) = raGrp.reserveAvailable(Time.minutes(15), target, goodCC)

    assertEquals(Time.Zero, rem)
    verifyReserve(newRes)
  }

  @Test def testGroupReserveAvailable() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg
    val (grp2, rem2) = Fixture.raResGroup.reserveAvailable(Time.minutes(15), target, goodCC)
    assertEquals(Time.Zero, rem2)
    verifyReserve(grp2.grp(target))
  }

  @Test def testReserve2() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg
    val prop   = mkProp(target, goodCC)
    val block  = Block(prop, prop.obsList.head, Time.minutes(15))

    val grp    = Fixture.raResGroup.grp(target).reserve(block, emptyQueue).toOption.get

    val target2 = Target(15.0, 45.0) // RA 1hr, Dec 45 deg
    val prop2   = mkProp(target2, badCC)
    val block2  = Block(prop2, prop2.obsList.head, Time.minutes(15))

    grp.reserve(block2, emptyQueue) match {
      case Left(msg)     => fail(msg.reason)
      case Right(newRes) =>
        // 2 obs of 15 minutes each take away 30 minutes from the total of 1 hr
        assertEquals(Time.minutes(60), newRes.limit)
        assertEquals(Time.minutes(30), newRes.remaining)
        assertFalse(newRes.isFull)

        // Dec range (0, 45] == 45 min still, but the absolute limit overrides
        assertEquals(Time.minutes(60), newRes.limit(target))
        assertEquals(Time.minutes(30), newRes.remaining(target))
        assertFalse(newRes.isFull(target))

        // Dec range (45, 90) is less than the overall limit
        assertEquals(Time.minutes(30), newRes.limit(target2))
        assertEquals(Time.minutes(15), newRes.remaining(target2))
        assertFalse(newRes.isFull(target2))

        // Took 15 minutes of the total of 30 for the Good CC conditions.
        assertEquals(Time.minutes(30), newRes.limit(goodCC))
        assertEquals(Time.minutes(15), newRes.remaining(goodCC))
        assertFalse(newRes.isFull(goodCC))

        // After two 15 min observations, there are only 30 min rem for bad CC
        assertEquals(Time.minutes(60), newRes.limit(badCC))
        assertEquals(Time.minutes(30), newRes.remaining(badCC))
        assertFalse(newRes.isFull(badCC))

        // Remaining for first target and conditions is therefore 15 mins
        // because the good CC is limited to 15 min.
        assertEquals(Time.minutes(30), newRes.limit(target, goodCC))
        assertEquals(Time.minutes(15), newRes.remaining(target, goodCC))
        assertFalse(newRes.isFull(target, goodCC))

        // Second target and conditions is also 15 min because the upper half
        // of the dec range is 15 min
        assertEquals(Time.minutes(30), newRes.limit(target2, badCC))
        assertEquals(Time.minutes(15), newRes.remaining(target2, badCC))
        assertFalse(newRes.isFull(target2, badCC))
    }
  }

  // Test that the absolute limit comes into play even if there is time
  // remaining for the obs conditions and dec.
  @Test def testOverPrescribeAbsolute() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg
    val prop = mkProp(target, badCC) // 1 hour limit for obs like this

    // Take almost all the time
    val block = Block(prop, prop.obsList.head, Time.minutes(59.9))
    val raRes = Fixture.raResGroup.grp(target).reserve(block, emptyQueue).toOption.get

    // badCC is being limited here by the absolute limit
    assertEquals(0.1, raRes.remaining(target, badCC).toMinutes.value, 0.000001)
    assertEquals(0.1, raRes.remaining(badCC).toMinutes.value, 0.000001)

    // Create a target in the upper half of the dec range.  It's bin has
    // 30 minutes left, not considering the absolute limit for the RA.
    val target2 = Target(15.0, 45.0) // RA 1hr, Dec 45 deg
    val prop2 = mkProp(target2, badCC) // 1 hour limit for obs like this

    // Try to schedule this block, which would work if it weren't for the
    // absolute limit.
    val block2 = Block(prop2, prop2.obsList.head, Time.minutes(0.11))
    raRes.reserve(block2, emptyQueue) match {
      case Left(msg: RejectTarget) => assertEquals(prop2, msg.prop)
      case _ => fail()
    }
  }

  @Test def testReserveAvailableLimitedByAbsoluteTimeForRA() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg

    val raRes1 = Fixture.raResGroup.grp(target)
    val (raRes2, rem2) = raRes1.reserveAvailable(Time.minutes(59.9), target, badCC)
    assertEquals(Time.Zero, rem2)

    assertEquals(0.1, raRes2.remaining(target, badCC).toMinutes.value, 0.000001)
    assertEquals(0.1, raRes2.remaining(badCC).toMinutes.value,         0.000001)

    // Create a target in the upper half of the dec range.  It's bin has
    // 30 minutes left, not considering the absolute limit for the RA.
    val target2 = Target(15.0, 45.0) // RA 1hr, Dec 45 deg
    val (raRes3, rem3) = raRes2.reserveAvailable(Time.minutes(0.11), target2, badCC)
    assertEquals(0.01, rem3.toMinutes.value, 0.000001)
    assertTrue(raRes3.isFull)
  }

  @Test def testOverPrescribeDec() = {
    val target = Target(15.0, 45.0)  // RA 1hr, Dec 45 deg -> 30 min dec
    val prop = mkProp(target, badCC) // 30 min dec limit, 1 hour conds limit

    // Take almost all the time for the dec
    val block = Block(prop, prop.obsList.head, Time.minutes(29.9))
    val raRes = Fixture.raResGroup.grp(target).reserve(block, emptyQueue).toOption.get

    assertEquals( 0.1, raRes.remaining(target, badCC).toMinutes.value, 0.000001)
    assertEquals(30.1, raRes.remaining(badCC).toMinutes.value, 0.000001)

    // Take it over the edge.
    val block2 = Block(prop, prop.obsList.head, Time.minutes(0.11))
    raRes.reserve(block2, emptyQueue) match {
      case Left(msg) => assertEquals(prop, msg.prop)
      case _ => fail()
    }
  }

  @Test def testReserveAvailableLimitedByDec() = {
    val target = Target(15.0, 45.0)  // RA 1hr, Dec 45 deg -> 30 min dec
    val raRes1 = Fixture.raResGroup.grp(target)

    // Take almost all the time for the dec
    val (raRes2, rem2) = raRes1.reserveAvailable(Time.minutes(29.9), target, badCC)
    assertEquals(Time.Zero, rem2)

    assertEquals( 0.1, raRes2.remaining(target, badCC).toMinutes.value, 0.000001)
    assertEquals(30.1, raRes2.remaining(badCC).toMinutes.value,         0.000001)

    // Take it over the edge.
    val (raRes3, rem3) = raRes2.reserveAvailable(Time.minutes(0.11), target, badCC)
    assertEquals( 0.01, rem3.toMinutes.value, 0.000001)
    assertTrue(raRes3.isFull(target))
    assertFalse(raRes3.isFull)
  }

  @Test def testOverPrescribeConds() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg -> 60 min dec limit
    val prop = mkProp(target, goodCC) // 60 min dec limit, 30 mins conds limit

    // Take almost all the time for the conditions
    val block = Block(prop, prop.obsList.head, Time.minutes(29.9))
    val raRes = Fixture.raResGroup.grp(target).reserve(block, emptyQueue).toOption.get

    assertEquals(0.1,  raRes.remaining(target, goodCC).toMinutes.value, 0.000001)
    assertEquals(30.1, raRes.remaining(target).toMinutes.value, 0.000001)

    // Take it over the edge.
    val block2 = Block(prop, prop.obsList.head, Time.minutes(0.11))
    raRes.reserve(block2, emptyQueue) match {
      case Left(msg: RejectConditions) => assertEquals(prop, msg.prop)
      case _ => fail()
    }
  }

  @Test def testReserveAvailableLimitedByConds() = {
    val target = Target(15.0, 0.0) // RA 1hr, Dec 0 deg -> 60 min dec limit
    val raRes1 = Fixture.raResGroup.grp(target)

    // Uses almost all the time available for good CC.
    val (raRes2, rem2) = raRes1.reserveAvailable(Time.minutes(29.9), target, goodCC)
    assertEquals(Time.Zero, rem2)

    assertEquals( 0.1, raRes2.remaining(target, goodCC).toMinutes.value, 0.000001)
    assertEquals(30.1, raRes2.remaining(target).toMinutes.value,         0.000001)

    // Take it over the edge.
    val (raRes3, rem3) = raRes2.reserveAvailable(Time.minutes(0.11), target, goodCC)
    assertEquals(0.01, rem3.toMinutes.value, 0.000001)
    assertTrue(raRes3.isFull(goodCC))
    assertFalse(raRes3.isFull)
  }

  @Test def testFull() = {
    val target = Target(0.0, 0.0) // RA 0 hr, Dec 0 deg -> 0 min dec limit
    assertTrue(Fixture.raResGroup.grp(target).isFull)
    assertTrue(Fixture.raResGroup.grp(target).isFull(target))
    assertTrue(Fixture.raResGroup.grp(target).isFull(goodCC))
    assertTrue(Fixture.raResGroup.grp(target).isFull(target, goodCC))
  }

  private case class TestCatTime(target: Target, conditions: ObservingConditions, time: Time) extends CategorizedTime

//  private val cat1 = TestCatTime(Target( 0.0, 0.0), badCC, Time.hours(10))
//  private val cat2 = TestCatTime(Target(15.0, 0.0), badCC, Time.hours(10))
//  private val cat3 = TestCatTime(Target(30.0, 0.0), badCC, Time.hours(10))

  @Test def testReserveAvailableWithEmptyList() = {
    val (newGrp, leftover) = Fixture.raResGroup.reserveAvailable(Nil)

    assertEquals(Time.Zero, leftover)
    val newList = newGrp.grp.bins
    val orgList = Fixture.raResGroup.grp.bins

    newList.zip(orgList) foreach { case (a, b) => assertEquals(a.absBounds, b.absBounds) }

  }
}
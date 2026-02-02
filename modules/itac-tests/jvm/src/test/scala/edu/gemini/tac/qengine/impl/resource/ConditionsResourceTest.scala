// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.ConditionsBin
import edu.gemini.tac.qengine.api.config.ConditionsBinGroup
import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectConditions
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site
import lucuma.core.util.Enumerated
import org.junit.*

import Assert.*
import CloudCover.*
import ImageQuality.IQ20
import SkyBackground.SB20
import WaterVapor.WV20
import Cat.*

class ConditionsResourceTest {
  import Partner.KR
  val partners = Enumerated[Partner].all

  private val bins = ConditionsBin.of(
      (Cat(Eq(CC50)),  Percent(25)),
      (Cat(Eq(CC70)),  Percent(25)),
      (Cat(Eq(CC80)),  Percent(25)),
      (Cat(Eq(CCAny)), Percent(25))
    )

  private val binGrp = ConditionsBinGroup.of(bins)
  private val resGrp = ConditionsResourceGroup(Time.minutes(100), binGrp)

  private val ntac   = Ntac(KR, "x", 0, Time.minutes(100)) // not used
  private val target = Target(0,0)                               // not used

  private def mkProp(obsConds: ObservingConditions): Proposal = {
    val obsList = List(Observation(null, target, obsConds, Time.minutes(10)))
    Proposal(ntac, site = Site.GS, obsList = obsList)
  }

  private def mkConds(cc: CloudCover): ObservingConditions =
    ObservingConditions(cc, IQ20, SB20, WV20)

  // Verify that the given remaining times match -- times must be specified
  // in order of CloudCover values.
  private def verifyTimes(track: ConditionsResourceGroup, mins: Int*) = {
    CloudCover.values.zip(mins).foreach {
      case (cc, min) => {
        val remaining = track.remaining(mkConds(cc))
        assertEquals(Time.minutes(min), remaining)
      }
    }
  }

  private def testSuccess(time: Time, cnds: ObservingConditions, mins: Int*) = {
    val (newResGrp, rem) = resGrp.reserveAvailable(time, cnds)
    assertEquals(Time.Zero, rem)
    verifyTimes(newResGrp, mins*)

    val prop  = mkProp(cnds)
    val otb   = Block(prop, prop.obsList.head, time)
    resGrp.reserve(otb, Fixture.emptyQueue) match {
      case Right(res) => verifyTimes(res, mins*)
      case _ => fail()
    }
  }

  @Test def testSimpleReservationThatRequiresNoTimeFromABetterBin() = {
    val time  = Time.minutes(10)
    val cnds  = mkConds(CC70)

    // Given 25 minutes for CC70, we should be able to fully reserve the time
    // in the corresponding bin.
    // CC50:  25 -  0 = 25 (25 <- 25)
    // CC70:  25 - 10 = 15 (40 <- 15 + 25)
    // CC80:  25 -  0 = 25 (65 <- 25 + 15 + 25)
    // Any.:  25 -  0 = 25 (90 <- 25 + 25 + 15 + 25)
    testSuccess(time, cnds, 25, 40, 65, 90)
  }

  @Test def testStealTimeFromABetterBin() = {
    val time  = Time.minutes(51)
    val cnds  = mkConds(CC80)

    // Given 51 minutes for CC80, we use all 25 minutes of CC80, all 25 of CC70,
    // and 1 minute of CC50.
    // CC50:  25 -  1 = 24 (24 <- 24)
    // CC70:  25 - 25 =  0 (24 <-  0 + 24)
    // CC80:  25 - 25 =  0 (24 <-  0 +  0 + 24)
    // Any.:  25 -  0 = 25 (49 <- 25 +  0 +  0 + 24)
    testSuccess(time, cnds, 24, 24, 24, 49)
  }

  @Test def testStealExactlyAllRemainingTimeFromBetterBins() = {
    val time  = Time.minutes(75)
    val cnds  = mkConds(CC80)

    // CC50:  25 - 25 =  0 ( 0 <-  0)
    // CC70:  25 - 25 =  0 ( 0 <-  0 +  0)
    // CC80:  25 - 25 =  0 ( 0 <-  0 +  0 +  0)
    // Any.:  25 -  0 = 25 (25 <- 25 +  0 +  0 + 0)
    testSuccess(time, cnds, 0, 0, 0, 25)
  }

  @Test def testAttemptToReserveMoreThanAvailable() = {
    val (newGrp, rem) = resGrp.reserveAvailable(Time.minutes(76), mkConds(CC80))
    verifyTimes(newGrp, 0, 0, 0, 25)
    assertEquals(Time.minutes(1), rem) // 1 minute could not be reserved
  }

  @Test def testCannotStealMoreThanThanAvailableFromBetterBins() = {
    val prop = mkProp(mkConds(CC80))

    val otb2 = Block(prop, prop.obsList.head, Time.minutes(76))
    resGrp.reserve(otb2, Fixture.emptyQueue) match {
      case Left(rc: RejectConditions) => // pass
      case _ => fail()
    }
  }

}
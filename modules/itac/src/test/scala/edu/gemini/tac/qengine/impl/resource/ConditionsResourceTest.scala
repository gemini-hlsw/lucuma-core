// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import cats.implicits.*
import edu.gemini.tac.qengine.api.config.ConditionsBin
import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.api.config.ConditionsCategoryMap
import edu.gemini.tac.qengine.impl.block.Block
import edu.gemini.tac.qengine.log.RejectConditions
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
import munit.FunSuite

import Cat.*

class ConditionsResourceTest extends FunSuite{
  import TimeAccountingCategory.KR
  val TimeAccountingCategorys = Enumerated[TimeAccountingCategory].all

  private val bins = ConditionsBin.of(
    CloudExtinction.Preset.values.map { ce =>
      (Cat(Eq(ce)), IntCentiPercent.unsafeFromPercent(100 / CloudExtinction.Preset.values.length))
    }*)

  private val binGrp = ConditionsCategoryMap.of(bins)
  private val resGrp = ConditionsCategoryMapResource(Time.minutes(100), binGrp)

  private val ntac   = Ntac(KR, "x", 0, Time.minutes(100)) // not used
  private val target = Target(0,0)                               // not used

  private def mkProp(obsConds: ConstraintSet): Proposal = {
    val obsList = List(Observation(target, obsConds, Time.minutes(10)))
    Proposal(ntac, site = Site.GS, obsList = obsList)
  }

  private def mkConds(cc: CloudExtinction.Preset): ConstraintSet =
    ConstraintSet(ImageQuality.Preset.PointOne, cc, SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)

  // Verify that the given remaining times match -- times must be specified
  // in order of CloudCover values.
  private def verifyTimes(track: ConditionsCategoryMapResource, mins: Int*) = {
    val expected = mins.map(m => Time.minutes(m.toDouble))
    val obtained = 
      CloudExtinction.Preset.values.toList.map: cc =>
        track.remaining(mkConds(cc))
    assertEquals(obtained.mkString(", "), expected.mkString(", ")) // diff is easier to read this way
  }

  private def testSuccess(time: Time, cnds: ConstraintSet, mins: Int*) = {
    val (newResGrp, rem) = resGrp.reserveAvailable(time, cnds)
    assertEquals(Time.Zero, rem)
    verifyTimes(newResGrp, mins*)

    val prop  = mkProp(cnds)
    val otb   = Block(prop, prop.obsList.head, time)
    resGrp.reserve(otb, Fixture.emptyQueue) match {
      case Right(res) => verifyTimes(res, mins*)
      case _ => fail("failed")
    }
  }

  test("testSimpleReservationThatRequiresNoTimeFromABetterBin") {
    val time  = Time.minutes(10)
    val cnds  = mkConds(CloudExtinction.Preset.PointThree)

    // Given 14 minutes for PointThree, we should be able to fully reserve the time
    // in the corresponding bin.
    // Zero          : 14 -  0 = 14 (+14 = 14)
    // PointOne      : 14 -  0 = 14 (+14 = 28)
    // PointThree    : 14 - 10 =  4 ( +4 = 32)
    // PointFive     : 14 -  0 = 14 (+14 = 46)
    // OnePointZero  : 14 -  0 = 14 (+14 = 60)
    // TwoPointZero  : 14 -  0 = 14 (+14 = 74)
    // ThreePointZero: 14 -  0 = 14 (+14 = 80)
    testSuccess(time, cnds, 14, 28, 32, 46, 60, 74, 88)
  }

  test("testStealTimeFromABetterBin") {
    val time  = Time.minutes(29)
    val cnds  = mkConds(CloudExtinction.Preset.OnePointZero)

    // Given 29 minutes for OnePointZero, we use all 14 minutes of OnePointZero, all 14 of PointFive,
    // and 1 minute of PointThree.
    // Zero          : 14 -  0 = 14 (+14 = 14) 
    // PointOne      : 14 -  0 = 14 (+14 = 28) 
    // PointThree    : 14 -  1 = 13 (+13 = 41) 
    // PointFive     : 14 - 14 =  0 (+ 0 = 41) 
    // OnePointZero  : 14 - 14 =  0 (+ 0 = 41) 
    // TwoPointZero  : 14 -  0 = 14 (+14 = 55) 
    // ThreePointZero: 14 -  0 = 14 (+14 = 69) 
    testSuccess(time, cnds, 14, 28, 41, 41, 41, 55, 69)
  }

  test("testStealExactlyAllRemainingTimeFromBetterBins") {
    val time  = Time.minutes(14 * 5)
    val cnds  = mkConds(CloudExtinction.Preset.OnePointZero)

    // Zero          : 14 - 14 =  0 (+ 0 =  0) 
    // PointOne      : 14 - 14 =  0 (+ 0 =  0) 
    // PointThree    : 14 - 14 =  0 (+ 0 =  0) 
    // PointFive     : 14 - 14 =  0 (+ 0 =  0) 
    // OnePointZero  : 14 - 14 =  0 (+ 0 =  0) 
    // TwoPointZero  : 14 -  0 = 14 (+14 = 14) 
    // ThreePointZero: 14 -  0 = 14 (+14 = 28) 
    testSuccess(time, cnds, 0, 0, 0, 0, 0, 14, 28)
  }

  test("testAttemptToReserveMoreThanAvailable") {
    val (newGrp, rem) = resGrp.reserveAvailable(Time.minutes(14 * 5 + 1), mkConds(CloudExtinction.Preset.OnePointZero))
    verifyTimes(newGrp, 0, 0, 0, 0, 0, 14, 28)
    assertEquals(Time.minutes(1), rem) // 1 minute could not be reserved
  }

  test("testCannotStealMoreThanThanAvailableFromBetterBins") {
    val prop = mkProp(mkConds(CloudExtinction.Preset.OnePointZero))

    val otb2 = Block(prop, prop.obsList.head, Time.minutes(76))
    resGrp.reserve(otb2, Fixture.emptyQueue) match {
      case Left(rc: RejectConditions) => // pass
      case _ => fail("failed")
    }
  }

}
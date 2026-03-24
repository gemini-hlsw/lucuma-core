// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import edu.gemini.tac.qengine.ItacSuite
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
import lucuma.core.util.Enumerated
import org.junit.*

import Assert.*

class TimeAccountingCategoryBlockIteratorTest extends ItacSuite {
  import TimeAccountingCategory.AR
  val TimeAccountingCategorys = Enumerated[TimeAccountingCategory].all

  val target = ItacTarget(0.0, 0.0) // required but not used for this test
  private val conds =
    ConstraintSet(
      ImageQuality.Preset.TwoPointZero,
      CloudExtinction.Preset.ThreePointZero,
      SkyBackground.Bright,
      WaterVapor.Wet,
      ElevationRange.ByAirMass.Default
    )
  val e      = 0.000001

  case class IdGen(num: Int = 0) {
    def id: String = "PropId" + num
    def next: IdGen = IdGen(num + 1)
  }

  var gen = IdGen()

  def mkObs(hrs: Double): ItacObservation = ItacObservation(target, conds, Time.hours(hrs))

  def mkProp(hrs: Double, obsHrs: Double*): Proposal = {
    val ntac = Ntac(AR, gen.id, 0, Time.hours(hrs))
    gen = gen.next
    val lst  = obsHrs.map(curHrs => ItacObservation(target, conds, Time.hours(curHrs))).toList
    Proposal(ntac, site = Site.GS, obsList = lst)
  }

  test("testCreateEmpty") {
    val it = TimeAccountingCategoryBlockIterator(Nil, _.obsList)
    assertEquals(false, it.hasNext)
  }

  private def validate(it: TimeAccountingCategoryBlockIterator,
                       propList: List[Proposal],
                       obsList: List[ItacObservation],
                       time: Double) = {

    assertEquals(propList, it.remainingProposals)
    assertEquals(obsList,  it.remainingObservationsInActiveList)
    assertEquals(time,     it.currentObservationRemainingTime.toHours.value, e)
    assertEquals(propList.head, it.currentProposal)
    assertEquals(obsList.head,  it.currentObservation)
  }

  test("testCreateSinglePropSingleObsFull") {
    val lst = List(mkProp(10, 10))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)
    assertTrue(it.hasNext)
    validate(it, lst, lst.head.obsList, 10)
  }

  test("testAdvancePartialObs") {
    val lst = List(mkProp(10, 10))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    val (block, it2) = it.next(Time.hours(5), _.obsList)
    assertTrue(it2.hasNext)
    validate(it2, lst, lst.head.obsList, 5)
    assertEquals(lst.head, block.prop)
    assertEquals(lst.head.obsList.head, block.obs)
    assertEquals(5.0, block.time.toHours.value, e)
  }

  test("testCanAdvancePartialObservationInBand3") {
    //NB: Note how this is structured exactly like above, but in B3
    val lst = List(mkProp(11, 10).copy(band3Observations = List(mkObs(11))))
    val it = TimeAccountingCategoryBlockIterator(lst, _.band3Observations)
    val (block, it2) = it.next(Time.hours(5), _.band3Observations)
    assertTrue(it2.hasNext)
    validate(it2, lst, lst.head.band3Observations, 6)
    assertEquals(lst.head, block.prop)
    assertEquals(lst.head.band3Observations.head, block.obs)
    assertEquals(5.0, block.time.toHours.value, e)
  }

  test("testTryAdvancePastObs") {
    val lst = List(mkProp(10, 10))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    val (block, it2) = it.next(Time.hours(15), _.obsList)
    assertFalse(it2.hasNext)
    assertEquals(lst.head, block.prop)
    assertEquals(lst.head.obsList.head, block.obs)
    assertEquals(10.0, block.time.toHours.value, e)
  }

  test("testAdvanceLastObs") {
    val lst = List(mkProp(10, 10))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    val (block, it2) = it.next(Time.hours(10), _.obsList)
    assertFalse(it2.hasNext)

    assertEquals(lst.head, block.prop)
    assertEquals(lst.head.obsList.head, block.obs)
    assertEquals(10.0, block.time.toHours.value, e)
  }

  test("testAdvanceObs") {
    val lst = List(mkProp(20, 10, 10))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    val (block1, it2) = it.next(Time.hours(15), _.obsList)
    assertTrue(it2.hasNext)
    validate(it2, lst, lst.head.obsList.tail, 10)

    assertEquals(lst.head, block1.prop)
    assertEquals(lst.head.obsList.head, block1.obs)
    assertEquals(10.0, block1.time.toHours.value, e)

    val (block2, it3) = it2.next(Time.hours(15), _.obsList)
    assertFalse(it3.hasNext)

    assertEquals(lst.head, block1.prop)
    assertEquals(lst.head.obsList.last, block2.obs)
    assertEquals(10.0, block2.time.toHours.value, e)
  }

  test("testAdvanceProp") {
    val lst = List(mkProp(20, 10, 10), mkProp(10, 5, 5))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    val tenHrs = Time.hours(10)
    val it2 = it.next(tenHrs, _.obsList)._2.next(tenHrs, _.obsList)._2
    validate(it2, lst.tail, lst.tail.head.obsList, 5)
  }

  test("testAdvanceToEnd") {
    val lst = List(mkProp(20, 10, 10), mkProp(10, 5, 5))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    val tenHrs = Time.hours(10)
    val it2 = it.next(tenHrs, _.obsList)._2.next(tenHrs, _.obsList)._2.next(tenHrs, _.obsList)._2.next(tenHrs, _.obsList)._2
    assertFalse(it2.hasNext)
  }

  test("testSkipFromBegining") {
    val lst = List(mkProp(20, 10, 10), mkProp(10, 5, 5))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)
    val it2 = it.skip(_.obsList)
    validate(it2, lst.tail, lst.tail.head.obsList, 5)
  }

  test("testSkipFromMiddle") {
    val lst = List(mkProp(20, 10, 10), mkProp(10, 5, 5))
    val it = TimeAccountingCategoryBlockIterator(lst, _.obsList)
    val it2 = it.next(Time.hours(10), _.obsList)._2.skip(_.obsList)
    validate(it2, lst.tail, lst.tail.head.obsList, 5)
  }

  test("testEmpty") {
    val it = TimeAccountingCategoryBlockIterator(Nil, _.obsList)
    val it2 = it.skip(_.obsList)
    assertFalse(it2.hasNext)
  }

  test("testStart") {
    val prop1 = mkProp(20, 10, 10)
    val prop2 = mkProp(10,  5,  5)
    val lst   = List(prop1, prop2)
    val it    = TimeAccountingCategoryBlockIterator(lst, _.obsList)

    assertTrue(it.isStartBlock)
    assertTrue(it.isStartOf(prop1))
    assertFalse(it.isStartOf(prop2))

    val fiveHrs = Time.hours(5)
    val it2 = it.next(fiveHrs, _.obsList)._2

    assertFalse(it2.isStartBlock)
    assertFalse(it2.isStartOf(prop1))
    assertFalse(it2.isStartOf(prop2))

    val it3 = it2.next(fiveHrs, _.obsList)._2

    assertFalse(it3.isStartBlock)
    assertFalse(it3.isStartOf(prop1))
    assertFalse(it3.isStartOf(prop2))

    val tenHrs = Time.hours(10)
    val it4 = it3.next(tenHrs, _.obsList)._2

    assertTrue(it4.isStartBlock)
    assertFalse(it4.isStartOf(prop1))
    assertTrue(it4.isStartOf(prop2))

    val it5 = it4.next(tenHrs, _.obsList)._2

    assertFalse(it5.isStartBlock)
    assertFalse(it5.isStartOf(prop1))
    assertFalse(it5.isStartOf(prop2))
  }
}
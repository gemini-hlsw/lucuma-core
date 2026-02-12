// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import edu.gemini.tac.qengine.api.queue.time.TimeAccountingCategoryTime
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.Enumerated
import org.junit.*

import Assert.*

class BlockIteratorTest {
  import TimeAccountingCategory.{ BR, US }
  val TimeAccountingCategorys = Enumerated[TimeAccountingCategory].all

  val target = Target(0.0, 0.0) // required but not used for this test
  val conds = ObservingConditions.AnyConditions // required by not used
  val e = 0.000001

  def mkObs(hrs: Double): Observation = Observation(target, conds, Time.hours(hrs))

  def mkProp(p: TimeAccountingCategory, hrs: Double, obsHrs: List[Double], b3ObsHrs: List[Double]): Proposal = {
    val ntac = Ntac(p, "na", 0, Time.hours(hrs))
    val lst = obsHrs.map(curHrs => Observation(target, conds, Time.hours(curHrs))).toList
    val b3obs = b3ObsHrs.map(curHrs => Observation(target, conds, Time.hours(curHrs))).toList
    Proposal(ntac, site = Site.GS, obsList = lst, band3Observations = b3obs)
  }

  def genQuanta(hrs: Double): TimeAccountingCategoryTime = TimeAccountingCategoryTime.constant(Time.hours(hrs))

  def genPropLists(count: Int, p: TimeAccountingCategory, propTime: Double, obsTimes: List[Double], b3ObsTimes: List[Double] = List.empty): Map[TimeAccountingCategory, List[Proposal]] = {
    val lst = (1 to count).map(_ => mkProp(p, propTime, obsTimes, b3ObsTimes)).toList
    TimeAccountingCategorys.map(p => (p, lst)).toMap
  }

  @Test def testEmptyQuanta() = {
    List[(Proposal) => List[Observation]](_.obsList, _.band3Observations).map {
      fn =>
        val it = BlockIterator(TimeAccountingCategoryTime.empty, List(US), genPropLists(1, US, 10, List(10), List(10)), fn)
        assertFalse(it.hasNext)
    }
    ()
  }

  // @Test def testZeroQuanta() = {
  //   List[(Proposal) => List[Observation]](_.obsList, _.band3Observations).foreach {
  //     fn =>
  //       val it = BlockIterator(genQuanta(0), List(US), genPropLists(1, US, 10, List(10)), fn)
  //       assertFalse(it.hasNext)
  //   }
  // }

  @Test def testEmptyPropListMap() = {
    val it = BlockIterator(genQuanta(10), List(US), Map.empty, _.obsList)
    assertFalse(it.hasNext)
  }

  @Test def testEmptyPropLists() = {
    List[(Proposal) => List[Observation]](_.obsList, _.band3Observations).map {
      fn =>
        val it = BlockIterator(genQuanta(10), List(US), genPropLists(0, US, 10, List(10)), fn)
        assertFalse(it.hasNext)
    }
    ()
  }

  // @Test def testEmptyTimeAccountingCategorySequence() = {
  //   List[(Proposal) => List[Observation]](_.obsList, _.band3Observations).map {
  //     fn =>
  //       val it = BlockIterator(genQuanta(10), Nil, genPropLists(1, US, 10, List(10)), fn)
  //       assertFalse(it.hasNext)
  //   }
  // }

  @Test def testTimeAccountingCategoryAdvanceNoProps() = {
    val prop = mkProp(US, 5, List(5), List.empty)
    val qMap = TimeAccountingCategoryTime.fromMap(Map(BR -> Time.hours(10), US -> Time.hours(10)))
    // No proposals for Brazil, it will be skipped.
    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(US -> List(prop))
    val it = BlockIterator(qMap, List(BR, US), pMap, _.obsList)

    val expected = List(Block(prop, prop.obsList.head, Time.hours(5), isStart = true, isFinal = true))
    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testTimeAccountingCategoryAdvanceNoTime() = {
    val brProp = mkProp(BR, 1, List(1), List.empty)
    val usProp = mkProp(US, 2, List(2), List.empty)

    // No time quantum for Brazil, it will be skipped.
    val qMap = TimeAccountingCategoryTime.fromMap(Map(US -> Time.hours(10)))
    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(qMap, List(BR, US), pMap, _.obsList)

    val expected = List(Block(usProp, usProp.obsList.head, Time.hours(2), isStart = true, isFinal = true))
    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testTimeAccountingCategoryAdvanceNoTimeIfOnlyB3() = {
    //Dup of above, but there are B3 observations
    val brProp = mkProp(BR, 1, List(1), List(1))
    val usProp = mkProp(US, 2, List(2), List.empty)

    // No time quantum for Brazil, it will be skipped.
    val qMap = TimeAccountingCategoryTime.fromMap(Map(US -> Time.hours(10)))
    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(qMap, List(BR, US), pMap, _.obsList)

    val expected = List(Block(usProp, usProp.obsList.head, Time.hours(2), isStart = true, isFinal = true))
    assertEquals(expected, it.toList(_.obsList))

  }

  @Test def testObsTimeLessThanQuantum() = {
    val brProp = mkProp(BR, 1, List(1), List.empty)
    val usProp = mkProp(US, 2, List(2), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))
    val it = BlockIterator(genQuanta(10), List(BR, US, BR, US), pMap, _.obsList)

    val expected = List(
      Block(brProp, brProp.obsList.head, Time.hours(1), isStart = true, isFinal = true),
      Block(usProp, usProp.obsList.head, Time.hours(2), isStart = true, isFinal = true)
    )

    val actual = it.toList(_.obsList)
    assertEquals(expected, actual)
  }

  @Test def testObsTimeSpansQuantum() = {
    val brProp = mkProp(BR, 11, List(11), List.empty)
    val usProp = mkProp(US, 5, List(5), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))
    val it = BlockIterator(genQuanta(10), List(BR, US, BR, US), pMap, _.obsList)

    val expected = List(
      Block(brProp, brProp.obsList.head, Time.hours(10), isStart = true, isFinal = false),
      Block(usProp, usProp.obsList.head, Time.hours(5), isStart = true, isFinal = true),
      Block(brProp, brProp.obsList.head, Time.hours(1), isStart = false, isFinal = true)
    )

    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testObsTimeEqualsQuantum() = {
    val brProp = mkProp(BR, 10, List(10), List.empty)
    val usProp = mkProp(US, 10, List(10), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))
    val it = BlockIterator(genQuanta(10), List(BR, US, BR, US), pMap, _.obsList)

    val expected = List(
      Block(brProp, brProp.obsList.head, Time.hours(10), isStart = true, isFinal = true),
      Block(usProp, usProp.obsList.head, Time.hours(10), isStart = true, isFinal = true)
    )

    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testMultipleObs() = {
    val brProp = mkProp(BR, 20, List[Double](10, 10), List.empty)
    val usProp = mkProp(US, 30, List[Double](10, 10, 10), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.obsList)

    // a time quantum of 15 hours will create:
    // BR Quantum 1
    //    All of first obs
    //    First half of second obs
    // US Quantum 1
    //    All of first obs
    //    First half of second obs
    // BR Quantum 2
    //    Last half of second obs -- final block in proposal
    // US Quantum 2
    //    Last half of second obs
    //    All of third obs -- final block in proposal

    val expected = List(
      Block(brProp, brProp.obsList.head, Time.hours(10), isStart = true, isFinal = false),
      Block(brProp, brProp.obsList.tail.head, Time.hours(5), isStart = false, isFinal = false),
      Block(usProp, usProp.obsList.head, Time.hours(10), isStart = true, isFinal = false),
      Block(usProp, usProp.obsList.tail.head, Time.hours(5), isStart = false, isFinal = false),
      Block(brProp, brProp.obsList.tail.head, Time.hours(5), isStart = false, isFinal = true),
      Block(usProp, usProp.obsList.tail.head, Time.hours(5), isStart = false, isFinal = false),
      Block(usProp, usProp.obsList.tail.tail.head, Time.hours(10), isStart = false, isFinal = true)
    )

    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testMultiplePropsInOneQuantum() = {
    val brProp1 = mkProp(BR, 2, List[Double](1, 1), List.empty)
    val brProp2 = mkProp(BR, 3, List(3), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp1, brProp2))

    val it = BlockIterator(genQuanta(5), List(BR, US), pMap, _.obsList)

    val expected = List(
      Block(brProp1, brProp1.obsList.head, Time.hours(1), isStart = true, isFinal = false),
      Block(brProp1, brProp1.obsList.tail.head, Time.hours(1), isStart = false, isFinal = true),
      Block(brProp2, brProp2.obsList.head, Time.hours(3), isStart = true, isFinal = true)
    )

    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testMultiplePropsSpanningQuantums() = {
    val brProp1 = mkProp(BR, 2, List[Double](1, 1), List.empty)
    val brProp2 = mkProp(BR, 3, List(3), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp1, brProp2))

    val it = BlockIterator(genQuanta(1), List(BR, BR, BR, BR, BR), pMap, _.obsList)

    val expected = List(
      Block(brProp1, brProp1.obsList.head, Time.hours(1), isStart = true, isFinal = false),
      Block(brProp1, brProp1.obsList.tail.head, Time.hours(1), isStart = false, isFinal = true),
      Block(brProp2, brProp2.obsList.head, Time.hours(1), isStart = true, isFinal = false),
      Block(brProp2, brProp2.obsList.head, Time.hours(1), isStart = false, isFinal = false),
      Block(brProp2, brProp2.obsList.head, Time.hours(1), isStart = false, isFinal = true)
    )

    assertEquals(expected, it.toList(_.obsList))
  }

  @Test def testSkipStartOfProposal() = {
    val brProp = mkProp(BR, 20, List[Double](10, 10), List.empty)
    val usProp = mkProp(US, 30, List[Double](10, 10, 10), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.obsList)

    // Gnerate the blocks from the first time quantum for BR
    val it2 = it.next(_.obsList)._2.next(_.obsList)._2
    assertEquals(US, it2.currentTimeAccountingCategory)
    val it3 = it2.skip(_.obsList)

    // Expect the last time quantum from BR
    val expected = List(
      Block(brProp, brProp.obsList.tail.head, Time.hours(5), isStart = false, isFinal = true)
    )

    assertEquals(expected, it3.toList(_.obsList))
  }

  @Test def testSkipMiddleOfProposal() = {
    val brProp = mkProp(BR, 20, List[Double](10, 10), List.empty)
    val usProp = mkProp(US, 30, List[Double](10, 10, 10), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.obsList)

    // Gnerate the blocks from the first time quantum for BR, and the first for
    // the US, skip the rest of the US proposal
    val it2 = it.next(_.obsList)._2.next(_.obsList)._2.next(_.obsList)._2.skip(_.obsList)

    // Expect the last time quantum from BR
    val expected = List(
      Block(brProp, brProp.obsList.tail.head, Time.hours(5), isStart = false, isFinal = true)
    )

    assertEquals(expected, it2.toList(_.obsList))
  }

  @Test def testRemainingProps() = {
    val brProp = mkProp(BR, 20, List[Double](10, 10), List.empty)
    val usProp = mkProp(US, 30, List[Double](10, 10, 10), List.empty)

    val pMap: Map[TimeAccountingCategory, List[Proposal]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.obsList)

    assertEquals(List(usProp, brProp).sortBy(_.toString), it.remPropList.sortBy(_.toString))

    //          BR(1)    BR(2)   US(1)   US(2)   BR(3)
    val it2 = it.next(_.obsList)._2.next(_.obsList)._2.next(_.obsList)._2.next(_.obsList)._2.next(_.obsList)._2

    // all of brazil's proposals generated
    assertEquals(List(usProp), it2.remPropList)

    //            US(3)   US(4)
    val it3 = it2.next(_.obsList)._2.next(_.obsList)._2

    assertEquals(Nil, it3.remPropList)
  }
}

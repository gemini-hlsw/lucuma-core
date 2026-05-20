// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import edu.gemini.tac.qengine.ItacSuite
import edu.gemini.tac.qengine.api.queue.time.TimeAccountingCategoryTime
import edu.gemini.tac.qengine.p1.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import org.junit.*

import Assert.*
import lucuma.core.enums.ScienceBand

class BlockIteratorTest extends ItacSuite {
  import TimeAccountingCategory.{ BR, US }
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
  val e = 0.000001

  def mkObs(hrs: Double): ItacObservation = ItacObservation(target, conds, TimeSpan.fromHoursBounded(hrs))

  def mkProp(p: TimeAccountingCategory, hrs: Double, obsHrs: List[Double]): ProposalShard =
    val ntac = Ntac(p, TimeSpan.fromHoursBounded(hrs))
    val lst = obsHrs.map(curHrs => ItacObservation(target, conds, TimeSpan.fromHoursBounded(curHrs))).toList
    val prop = Proposal(ProposalReference(Semester(YearInt.unsafeFrom(2026), Half.A), PosInt.unsafeFrom(1)), ntac, obsList = lst)
    prop.shardFor(Site.GN, p, ScienceBand.Band1)

  def genQuanta(hrs: Double): TimeAccountingCategoryTime = TimeAccountingCategoryTime.constant(TimeSpan.fromHoursBounded(hrs))

  def genPropLists(count: Int, p: TimeAccountingCategory, propTime: Double, obsTimes: List[Double]): Map[TimeAccountingCategory, List[ProposalShard]] =
    val lst = (1 to count).map(_ => mkProp(p, propTime, obsTimes)).toList
    TimeAccountingCategorys.map(p => (p, lst)).toMap

  test("testEmptyQuanta") {
    List[(ProposalShard) => List[ItacObservation.Scaled]](_.observations).map {
      fn =>
        val it = BlockIterator(TimeAccountingCategoryTime.empty, List(US), genPropLists(1, US, 10, List(10)), fn)
        assertFalse(it.hasNext)
    }
    ()
  }

  test("testZeroQuanta") {
    List[(ProposalShard) => List[ItacObservation.Scaled]](_.observations).foreach {
      fn =>
        val it = BlockIterator(genQuanta(0), List(US), genPropLists(1, US, 10, List(10)), fn)
        assertFalse(it.hasNext)
    }
  }

  test("testEmptyPropListMap") {
    val it = BlockIterator(genQuanta(10), List(US), Map.empty, _.observations)
    assertFalse(it.hasNext)
  }

  test("testEmptyPropLists") {
    List[(ProposalShard) => List[ItacObservation.Scaled]](_.observations).map {
      fn =>
        val it = BlockIterator(genQuanta(10), List(US), genPropLists(0, US, 10, List(10)), fn)
        assertFalse(it.hasNext)
    }
    ()
  }

  // test("testEmptyTimeAccountingCategorySequence") {
  //   List[(ProposalShard) => List[ItacObservation.Scaled]](_.observations).map {
  //     fn =>
  //       val it = BlockIterator(genQuanta(10), Nil, genPropLists(1, US, 10, List(10)), fn)
  //       assertFalse(it.hasNext)
  //   }
  // }

  test("testTimeAccountingCategoryAdvanceNoProps") {
    val prop = mkProp(US, 5, List(5))
    val qMap = TimeAccountingCategoryTime.fromMap(Map(BR -> TimeSpan.fromHoursBounded(10), US -> TimeSpan.fromHoursBounded(10)))
    // No proposals for Brazil, it will be skipped.
    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(US -> List(prop))
    val it = BlockIterator(qMap, List(BR, US), pMap, _.observations)

    val expected = List(Block(prop, prop.observations.head, TimeSpan.fromHoursBounded(5), isStart = true, isFinal = true))
    assertEquals(expected, it.toList(_.observations))
  }

  test("testTimeAccountingCategoryAdvanceNoTime") {
    val brProp = mkProp(BR, 1, List(1))
    val usProp = mkProp(US, 2, List(2))

    // No time quantum for Brazil, it will be skipped.
    val qMap = TimeAccountingCategoryTime.fromMap(Map(US -> TimeSpan.fromHoursBounded(10)))
    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(qMap, List(BR, US), pMap, _.observations)

    val expected = List(Block(usProp, usProp.observations.head, TimeSpan.fromHoursBounded(2), isStart = true, isFinal = true))
    assertEquals(expected, it.toList(_.observations))
  }

  // test("testTimeAccountingCategoryAdvanceNoTimeIfOnlyB3") {
  //   //Dup of above, but there are B3 observations
  //   val brProp = mkProp(BR, 1, List(1), List(1))
  //   val usProp = mkProp(US, 2, List(2), List.empty)

  //   // No time quantum for Brazil, it will be skipped.
  //   val qMap = TimeAccountingCategoryTime.fromMap(Map(US -> TimeSpan.fromHoursBounded(10)))
  //   val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))

  //   val it = BlockIterator(qMap, List(BR, US), pMap, _.observations)

  //   val expected = List(Block(usProp, usProp.observations.head, TimeSpan.fromHoursBounded(2), isStart = true, isFinal = true))
  //   assertEquals(expected, it.toList(_.observations))

  // }

  test("testObsTimeLessThanQuantum") {
    val brProp = mkProp(BR, 1, List(1))
    val usProp = mkProp(US, 2, List(2))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))
    val it = BlockIterator(genQuanta(10), List(BR, US, BR, US), pMap, _.observations)

    val expected = List(
      Block(brProp, brProp.observations.head, TimeSpan.fromHoursBounded(1), isStart = true, isFinal = true),
      Block(usProp, usProp.observations.head, TimeSpan.fromHoursBounded(2), isStart = true, isFinal = true)
    )

    val actual = it.toList(_.observations)
    assertEquals(expected, actual)
  }

  test("testObsTimeSpansQuantum") {
    val brProp = mkProp(BR, 11, List(11))
    val usProp = mkProp(US, 5, List(5))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))
    val it = BlockIterator(genQuanta(10), List(BR, US, BR, US), pMap, _.observations)

    val expected = List(
      Block(brProp, brProp.observations.head, TimeSpan.fromHoursBounded(10), isStart = true, isFinal = false),
      Block(usProp, usProp.observations.head, TimeSpan.fromHoursBounded(5), isStart = true, isFinal = true),
      Block(brProp, brProp.observations.head, TimeSpan.fromHoursBounded(1), isStart = false, isFinal = true)
    )

    assertEquals(expected, it.toList(_.observations))
  }

  test("testObsTimeEqualsQuantum") {
    val brProp = mkProp(BR, 10, List(10))
    val usProp = mkProp(US, 10, List(10))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))
    val it = BlockIterator(genQuanta(10), List(BR, US, BR, US), pMap, _.observations)

    val expected = List(
      Block(brProp, brProp.observations.head, TimeSpan.fromHoursBounded(10), isStart = true, isFinal = true),
      Block(usProp, usProp.observations.head, TimeSpan.fromHoursBounded(10), isStart = true, isFinal = true)
    )

    assertEquals(expected, it.toList(_.observations))
  }

  test("testMultipleObs") {
    val brProp = mkProp(BR, 20, List[Double](10, 10))
    val usProp = mkProp(US, 30, List[Double](10, 10, 10))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.observations)

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
      Block(brProp, brProp.observations.head, TimeSpan.fromHoursBounded(10), isStart = true, isFinal = false),
      Block(brProp, brProp.observations.tail.head, TimeSpan.fromHoursBounded(5), isStart = false, isFinal = false),
      Block(usProp, usProp.observations.head, TimeSpan.fromHoursBounded(10), isStart = true, isFinal = false),
      Block(usProp, usProp.observations.tail.head, TimeSpan.fromHoursBounded(5), isStart = false, isFinal = false),
      Block(brProp, brProp.observations.tail.head, TimeSpan.fromHoursBounded(5), isStart = false, isFinal = true),
      Block(usProp, usProp.observations.tail.head, TimeSpan.fromHoursBounded(5), isStart = false, isFinal = false),
      Block(usProp, usProp.observations.tail.tail.head, TimeSpan.fromHoursBounded(10), isStart = false, isFinal = true)
    )

    assertEquals(expected, it.toList(_.observations))
  }

  test("testMultiplePropsInOneQuantum") {
    val brProp1 = mkProp(BR, 2, List[Double](1, 1))
    val brProp2 = mkProp(BR, 3, List(3))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp1, brProp2))

    val it = BlockIterator(genQuanta(5), List(BR, US), pMap, _.observations)

    val expected = List(
      Block(brProp1, brProp1.observations.head, TimeSpan.fromHoursBounded(1), isStart = true, isFinal = false),
      Block(brProp1, brProp1.observations.tail.head, TimeSpan.fromHoursBounded(1), isStart = false, isFinal = true),
      Block(brProp2, brProp2.observations.head, TimeSpan.fromHoursBounded(3), isStart = true, isFinal = true)
    )

    assertEquals(expected, it.toList(_.observations))
  }

  test("testMultiplePropsSpanningQuantums") {
    val brProp1 = mkProp(BR, 2, List[Double](1, 1))
    val brProp2 = mkProp(BR, 3, List(3))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp1, brProp2))

    val it = BlockIterator(genQuanta(1), List(BR, BR, BR, BR, BR), pMap, _.observations)

    val expected = List(
      Block(brProp1, brProp1.observations.head, TimeSpan.fromHoursBounded(1), isStart = true, isFinal = false),
      Block(brProp1, brProp1.observations.tail.head, TimeSpan.fromHoursBounded(1), isStart = false, isFinal = true),
      Block(brProp2, brProp2.observations.head, TimeSpan.fromHoursBounded(1), isStart = true, isFinal = false),
      Block(brProp2, brProp2.observations.head, TimeSpan.fromHoursBounded(1), isStart = false, isFinal = false),
      Block(brProp2, brProp2.observations.head, TimeSpan.fromHoursBounded(1), isStart = false, isFinal = true)
    )

    assertEquals(expected, it.toList(_.observations))
  }

  test("testSkipStartOfProposal") {
    val brProp = mkProp(BR, 20, List[Double](10, 10))
    val usProp = mkProp(US, 30, List[Double](10, 10, 10))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.observations)

    // Gnerate the blocks from the first time quantum for BR
    val it2 = it.next(_.observations)._2.next(_.observations)._2
    assertEquals(US, it2.currentTimeAccountingCategory)
    val it3 = it2.skip(_.observations)

    // Expect the last time quantum from BR
    val expected = List(
      Block(brProp, brProp.observations.tail.head, TimeSpan.fromHoursBounded(5), isStart = false, isFinal = true)
    )

    assertEquals(expected, it3.toList(_.observations))
  }

  test("testSkipMiddleOfProposal") {
    val brProp = mkProp(BR, 20, List[Double](10, 10))
    val usProp = mkProp(US, 30, List[Double](10, 10, 10))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.observations)

    // Gnerate the blocks from the first time quantum for BR, and the first for
    // the US, skip the rest of the US proposal
    val it2 = it.next(_.observations)._2.next(_.observations)._2.next(_.observations)._2.skip(_.observations)

    // Expect the last time quantum from BR
    val expected = List(
      Block(brProp, brProp.observations.tail.head, TimeSpan.fromHoursBounded(5), isStart = false, isFinal = true)
    )

    assertEquals(expected, it2.toList(_.observations))
  }

  test("testRemainingProps") {
    val brProp = mkProp(BR, 20, List[Double](10, 10))
    val usProp = mkProp(US, 30, List[Double](10, 10, 10))

    val pMap: Map[TimeAccountingCategory, List[ProposalShard]] = Map(BR -> List(brProp), US -> List(usProp))

    val it = BlockIterator(genQuanta(15), List(BR, US, BR, US), pMap, _.observations)

    assertEquals(List(usProp, brProp).sortBy(_.toString), it.remPropList.sortBy(_.toString))

    //          BR(1)    BR(2)   US(1)   US(2)   BR(3)
    val it2 = it.next(_.observations)._2.next(_.observations)._2.next(_.observations)._2.next(_.observations)._2.next(_.observations)._2

    // all of brazil's proposals generated
    assertEquals(List(usProp), it2.remPropList)

    //            US(3)   US(4)
    val it3 = it2.next(_.observations)._2.next(_.observations)._2

    assertEquals(Nil, it3.remPropList)
  }
}

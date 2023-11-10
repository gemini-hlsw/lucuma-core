// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class TimestampIntervalSuite extends DisciplineSuite {

  import arb.ArbTimestampInterval.given

  checkAll("Order", OrderTests[TimestampInterval].order)

  import TimestampInterval.Overlap

  private def timestamp(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli).get

  private def interval(epochMilli0: Long, epochMilli1: Long): TimestampInterval =
    TimestampInterval.between(timestamp(epochMilli0), timestamp(epochMilli1))

  private def testOverlap(ex: Overlap, ts: (TimestampInterval, TimestampInterval)*): Unit =
    ts.foreach { case (a, b) => assertEquals(a.overlap(b), ex) }

  test("overlap => None") {
    testOverlap(Overlap.None,
      (interval( 0, 10), interval(20, 30)),
      (interval(20, 30), interval( 0, 10)),
      (interval( 0, 10), interval(10, 20)),
      (interval(10, 20), interval( 0, 10))
    )
  }

  test("overlap => LowerPartial") {
    testOverlap(Overlap.LowerPartial,
      (interval(0, 10), interval(1, 11)),
      (interval(0, 10), interval(9, 11))
    )
  }

  test("overlap => UpperPartial") {
    testOverlap(Overlap.UpperPartial,
      (interval(1, 11), interval(0, 10)),
      (interval(9, 11), interval(0, 10))
    )
  }

  test("overlap => Equal") {
    testOverlap(Overlap.Equal,
      (interval(0, 10), interval(0, 10)),
      (interval(0,  0), interval(0,  0))
    )
  }

  test("overlap => ProperSubset") {
    testOverlap(Overlap.ProperSubset,
      (interval(1, 9), interval(0, 10)),
      (interval(5, 5), interval(0, 10))
    )
  }

  test("overlap => ProperSuperset") {
    testOverlap(Overlap.ProperSuperset,
      (interval(0, 10), interval(1, 9)),
      (interval(0, 10), interval(5, 5))
    )
  }

  test("overlap reverse") {
    forAll { (t0: TimestampInterval, t1: TimestampInterval) =>
      t0.overlap(t1) match {
        case Overlap.ProperSubset   => assertEquals(Overlap.ProperSuperset, t1.overlap(t0))
        case Overlap.ProperSuperset => assertEquals(Overlap.ProperSubset,   t1.overlap(t0))
        case Overlap.None           => assertEquals(Overlap.None,           t1.overlap(t0))
        case Overlap.Equal          => assertEquals(Overlap.Equal,          t1.overlap(t0))
        case Overlap.LowerPartial   => assertEquals(Overlap.UpperPartial,   t1.overlap(t0))
        case Overlap.UpperPartial   => assertEquals(Overlap.LowerPartial,   t1.overlap(t0))
      }
    }

  }

  test("span") {
    assertEquals(interval(0, 10).span(interval(5, 5)), interval(0, 10))
    assertEquals(interval(0, 10).span(interval(1, 9)), interval(0, 10))

    assertEquals(interval(0, 10).span(interval(1, 11)), interval(0, 11))
    assertEquals(interval(1, 11).span(interval(0, 10)), interval(0, 11))
  }

  test("span is commutative") {
    forAll { (t0: TimestampInterval, t1: TimestampInterval) =>
      assertEquals(t0.span(t1), t1.span(t0))
    }
  }

  test("span is associative") {
    forAll { (t0: TimestampInterval, t1: TimestampInterval, t2: TimestampInterval) =>
      assertEquals(t0.span(t1).span(t2), t0.span(t1.span(t2)))
    }
  }

  test("plus") {
    assertEquals(interval(0, 10).plus(interval(10, 20)), List(interval(0, 20)))
    assertEquals(interval(0, 10).plus(interval( 9, 20)), List(interval(0, 20)))
    assertEquals(interval(0, 10).plus(interval(20, 30)), List(interval(0, 10), interval(20, 30)))
  }

  test("plus is commutative") {
    forAll { (t0: TimestampInterval, t1: TimestampInterval) =>
      assertEquals(t0.plus(t1), t1.plus(t0))
    }
  }

  test("minus") {
    // Lower Partial
    assertEquals(interval(1, 10).minus(interval(0,  2)), List(interval(2, 10)))

    // Upper Partial
    assertEquals(interval(1, 10).minus(interval(9, 11)), List(interval(1, 9)))

    // Proper Superset
    assertEquals(interval(0, 10).minus(interval(3,  6)), List(interval(0, 3), interval(6, 10)))

    // None
    assertEquals(interval(0, 10).minus(interval(10, 20)), List(interval(0, 10)))

    // Proper Subset
    assertEquals(interval(3, 6).minus(interval(0, 10)), Nil)

    // Equal
    assertEquals(interval(0, 10).minus(interval(0, 10)), Nil)
  }

}

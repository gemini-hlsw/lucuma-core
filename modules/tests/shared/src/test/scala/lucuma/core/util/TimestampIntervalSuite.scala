// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.syntax.option.*
import lucuma.core.util.arb.ArbTimestamp.given
import lucuma.core.util.arb.ArbTimestampInterval.given
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class TimestampIntervalSuite extends DisciplineSuite {

  checkAll("Order", OrderTests[TimestampInterval].order)

  import TimestampInterval.Overlap

  private def timestamp(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli).get

  private def interval(epochMilli0: Long, epochMilli1: Long): TimestampInterval =
    TimestampInterval.between(timestamp(epochMilli0), timestamp(epochMilli1))

  private def timeSpan(milli: Long): TimeSpan =
    TimeSpan.unsafeFromMicroseconds(milli * 1000)

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
    assertEquals(interval(0, 10).minus(interval(3, 6)), List(interval(0, 3), interval(6, 10)))
    assertEquals(interval(0, 10).minus(interval(0, 6)), List(interval(6, 10)))
    assertEquals(interval(0, 10).minus(interval(3, 10)), List(interval(0, 3)))
    // with empty intervals 
    assertEquals(interval(0, 10).minus(interval(0, 0)), List(interval(0, 10)))
    assertEquals(interval(0, 10).minus(interval(3, 3)), List(interval(0, 10)))

    // None
    assertEquals(interval(0, 10).minus(interval(10, 20)), List(interval(0, 10)))
    // with empty intervals
    assertEquals(interval(10, 10).minus(interval(0, 10)), List(interval(10, 10)))
    assertEquals(interval(0, 10).minus(interval(10, 10)), List(interval(0, 10)))


    // Proper Subset
    assertEquals(interval(3, 6).minus(interval(0, 10)), Nil)
    // with empty intervals
    assertEquals(interval(0, 0).minus(interval(0, 10)), Nil)
    assertEquals(interval(3, 3).minus(interval(0, 10)), Nil)

    // Equal
    assertEquals(interval(0, 10).minus(interval(0, 10)), Nil)
    assertEquals(interval(1, 1).minus(interval(1, 1)), Nil)
  }

  test("intersection") {
    // Lower Partial
    assertEquals(interval(1, 10).intersection(interval(0,  2)), Some(interval(1, 2)))

    // Upper Partial
    assertEquals(interval(1, 10).intersection(interval(9, 11)), Some(interval(9, 10)))

    // Proper Superset
    assertEquals(interval(0, 10).intersection(interval(3,  6)), Some(interval(3, 6)))

    // None
    assertEquals(interval(0, 10).intersection(interval(10, 20)), None)

    // Proper Subset
    assertEquals(interval(3, 6).intersection(interval(0, 10)), Some(interval(3, 6)))

    // Equal
    assertEquals(interval(0, 10).intersection(interval(0, 10)), Some(interval(0, 10)))
  }

  test("timeBetween") {
    // Lower Partial
    assertEquals(interval(1, 10).timeBetween(interval(0,  2)), Some(TimeSpan.Zero))

    // Upper Partial
    assertEquals(interval(1, 10).timeBetween(interval(9, 11)), Some(TimeSpan.Zero))

    // Proper Superset
    assertEquals(interval(0, 10).timeBetween(interval(3,  6)), Some(TimeSpan.Zero))

    // Proper Subset
    assertEquals(interval(3, 6).timeBetween(interval(0, 10)), Some(TimeSpan.Zero))

    // Equal
    assertEquals(interval(0, 10).timeBetween(interval(0, 10)), Some(TimeSpan.Zero))

    // Abuts, in order
    assertEquals(interval(0, 10).timeBetween(interval(10, 20)), Some(TimeSpan.Zero))

    // Abuts, out of order
    assertEquals(interval(10, 20).timeBetween(interval(0, 10)), Some(TimeSpan.Zero))

    // Disjoint, in order
    assertEquals(interval(0, 10).timeBetween(interval(15, 20)), Some(timeSpan(5)))

    // Disjoint, out of order
    assertEquals(interval(29, 30).timeBetween(interval(0, 10)), Some(timeSpan(19)))

    val oor = TimestampInterval.between(Timestamp.Max, Timestamp.Max)

    // Out of range, in order
    assertEquals(interval(0, 1).timeBetween(oor), None)

    // Out of range, out of order
    assertEquals(oor.timeBetween(interval(0, 10)), None)
  }

  test("until abuts from") {
    forAll { (t: Timestamp) =>
      assert(TimestampInterval.until(t).abuts(TimestampInterval.from(t)))
    }
  }

  test("until + from = All") {
    forAll { (t: Timestamp) =>
      assertEquals(
        TimestampInterval.until(t).span(TimestampInterval.from(t)),
        TimestampInterval.All
      )
    }
  }

  test("between works in either order") {
    forAll { (t0: Timestamp, t1: Timestamp) =>
      assertEquals(
        TimestampInterval.between(t0, t1),
        TimestampInterval.between(t1, t0)
      )
    }
  }

  test("All ∩ any = any") {
    forAll { (i: TimestampInterval) =>
      assertEquals(
        TimestampInterval.All.intersection(i),
        i.some
      )
    }
  }

  test("interval = from(start) ∩ until(end)") {
    forAll { (i: TimestampInterval) =>
      assertEquals(
        TimestampInterval.from(i.start).intersection(TimestampInterval.until(i.end)),
        i.some
      )
    }
  }

  test("All - any = until(start), from(end)") {
    forAll { (i: TimestampInterval) =>
      val first    = TimestampInterval.until(i.start)
      val last     = TimestampInterval.from(i.end)
      val expected =
        if (first.abuts(last)) List(TimestampInterval.All)
        else List(first, last).filter(_.nonEmpty)

      assertEquals(
        TimestampInterval.All.minus(i),
        expected
      )
    }
  }
}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Order.catsKernelOrderingForOrder
import cats.kernel.laws.discipline.*
import cats.syntax.foldable.*
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

class TimestampUnionSuite extends DisciplineSuite {

  import arb.ArbTimestamp.*
  import arb.ArbTimestampInterval.given
  import arb.ArbTimestampUnion.given

  checkAll("Eq", EqTests[TimestampUnion].eqv)

  private def timestamp(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli).get

  private def interval(epochMilli0: Long, epochMilli1: Long): TimestampInterval =
    TimestampInterval.between(timestamp(epochMilli0), timestamp(epochMilli1))

  private def expect(u: TimestampUnion, expected: List[TimestampInterval]): Unit =
    assertEquals(u.intervals.toList, expected, s"$u")

  test("add one") {
    expect(TimestampUnion(interval(1, 3)), List(interval(1, 3)))
  }

  test("add disjoint") {
    expect(TimestampUnion(interval(1, 2), interval(4, 5)), List(interval(1, 2), interval(4, 5)))
    expect(TimestampUnion(interval(4, 5), interval(1, 2)), List(interval(1, 2), interval(4, 5)))
  }

  test("add abutting") {
    expect(TimestampUnion(interval(1, 2), interval(2, 3)), List(interval(1, 3)))
  }

  test("add two partial overlapping") {
    expect(TimestampUnion(interval(1, 3), interval(2, 4)), List(interval(1, 4)))
    expect(TimestampUnion(interval(2, 4), interval(1, 3)), List(interval(1, 4)))
  }

  test("add two completely overlapping") {
    expect(TimestampUnion(interval(1, 4), interval(2, 3)), List(interval(1, 4)))
    expect(TimestampUnion(interval(2, 3), interval(1, 4)), List(interval(1, 4)))
  }

  test("remove all") {
    expect(TimestampUnion(interval(1,3)).remove(interval(1,3)), Nil)
  }

  test("remove nothing") {
    expect(TimestampUnion(interval(1,3)).remove(interval(5, 6)), List(interval(1, 3)))
  }

  test("remove in middle") {
    expect(TimestampUnion(interval(1, 4)).remove(interval(2, 3)), List(interval(1, 2), interval(3, 4)))
  }

  test("add order is irrelevant") {
    forAll { (u: TimestampUnion, ts: List[TimestampInterval]) =>
      val tsʹ = ts.take(10)
      assertEquals(u ++ tsʹ, u ++ tsʹ.sorted)
    }
  }

  test("remove order is irrelevant") {
    forAll { (u: TimestampUnion, ts: List[TimestampInterval]) =>
      assertEquals(u -- ts.take(10), u -- ts.take(10).sorted)
    }
  }

  test("intersect") {
    forAll { (u0: TimestampUnion, u1: TimestampUnion, ts: List[Timestamp]) =>
      val u2 = u0 ∩ u1
      ts.foreach { t =>
        assertEquals(u2.contains(t), u0.contains(t) && u1.contains(t), s"intersect: $u0 ∩ $u1 = $u2, t=$t, u0(${u0.contains(t)}), u1(${u1.contains(t)}), u2(${u2.contains(t)})")
      }
    }
  }

  test("boundedSum") {
    forAll { (u: TimestampUnion) =>
      assertEquals(u.boundedSum, u.intervals.foldMap(_.boundedTimeSpan))
    }
  }
}

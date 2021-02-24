// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import cats.syntax.all._
import java.time.Instant
import java.time.Duration
import java.time.LocalTime
import java.time.ZoneId
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import lucuma.core.math.arb._
import io.chrisdavenport.cats.time._
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.OrderTests
import monocle.law.discipline.PrismTests
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.core.arb.ArbTime

final class IntervalSuite extends munit.DisciplineSuite with IntervalGens {
  import ArbInterval._
  import ArbSchedule._
  import ArbTime._

  test("Equality must be natural") {
    forAll { (a: Interval, b: Interval) =>
      assertEquals(a.equals(b), Eq[Interval].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Interval) =>
      assertEquals(a.toString, Show[Interval].show(a))
    }
  }

  // Laws
  checkAll("Eq", EqTests[Interval].eqv)
  checkAll("Order", OrderTests[Interval].order)

  // Optics
  checkAll("fromOrderedInstants", PrismTests(Interval.fromOrderedInstants))
  checkAll("fromInstants", FormatTests(Interval.fromInstants).format)
  checkAll("fromStartDuration", PrismTests(Interval.fromStartDuration))

  test("Contains Instant") {
    forAll { i: Interval =>
      forAll(instantInInterval(i)) { inst: Instant =>
        assert(i.contains(inst))
      }
    }
  }

  test("Not Contains Instant") {
    forAll { i: Interval =>
      forAll(instantOutsideInterval(i)) { instOpt =>
        assert(instOpt.forall(inst => !i.contains(inst)))
      }
    }
  }

  test("Contains Interval") {
    forAll { i: Interval =>
      forAll(
        distinctZip(instantInInterval(i, includeEnd = true),
                    instantInInterval(i, includeEnd = true)
        )
      ) { instants =>
        assert(Interval.fromInstants.getOption(instants).exists(i.contains))
      }
    }
  }

  test("Not Contains Interval") {
    forAll { i: Interval =>
      forAll(
        // At least one instant not in Interval
        distinctZipOpt(Gen.some(instantWithSpecialInterval(i)),
                       instantOutsideInterval(i, includeEnd = false)
        )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(other => !i.contains(other))
        )
      }
    }
  }

  test("Abuts") {
    forAll { i: Interval =>
      forAll(
        Gen.oneOf(
          instantBeforeInterval(i).map(_.map(s => Interval.unsafe(s, i.start))),
          instantAfterInterval(i, includeEnd = false)
            .map(_.map(e => Interval.unsafe(i.end, e)))
        )
      ) { i2Opt =>
        assert(i2Opt.forall(i.abuts))
      }
    }
  }

  test("Not Abuts") {
    forAll { i: Interval =>
      forAll(
        arbitrary[Interval]
          .suchThat(i2 => catsSyntaxEq(i2.end) =!= i.start)
          .suchThat(i2 => catsSyntaxEq(i2.start) =!= i.end)
      ) { i2: Interval =>
        assert(!i.abuts(i2))
      }
    }
  }

  test("Overlaps") {
    forAll { i: Interval =>
      forAll(
        distinctZip(instantUntilEndOfInterval(i),
                    instantFromStartOfInterval(i, includeStart = false)
        )
      ) { instants =>
        assert(Interval.fromInstants.getOption(instants).exists(i.overlaps))
      }
    }
  }

  test("Not Overlaps") {
    forAll { i: Interval =>
      forAll(
        Gen
          .oneOf(
            distinctZipOpt(instantBeforeInterval(i), instantBeforeInterval(i)),
            distinctZipOpt(instantAfterInterval(i), instantAfterInterval(i))
          )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(other => !i.overlaps(other))
        )
      }
    }
  }

  test("Join") {
    forAll { i: Interval =>
      forAll(
        distinctZip(instantUntilEndOfInterval(i, includeEnd = true), instantFromStartOfInterval(i))
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          val join = i.join(other)
          assertEquals(join.map(_.start), List(i.start, other.start).min.some)
          assertEquals(join.map(_.end), List(i.end, other.end).max.some)
        }
      }
    }
  }

  test("Empty Join") {
    forAll { i: Interval =>
      forAll(
        Gen
          .oneOf(
            distinctZipOpt(instantBeforeInterval(i), instantBeforeInterval(i)),
            distinctZipOpt(instantAfterInterval(i, includeEnd = false),
                           instantAfterInterval(i, includeEnd = false)
            )
          )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(other => i.join(other).isEmpty)
        )
      }
    }
  }

  test("Intersection") {
    forAll { i: Interval =>
      forAll(
        distinctZip(instantUntilEndOfInterval(i),
                    instantFromStartOfInterval(i, includeStart = false)
        )
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          val intersection = i.intersection(other)
          assertEquals(intersection.map(_.start), List(i.start, other.start).max.some)
          assertEquals(intersection.map(_.end), List(i.end, other.end).min.some)
        }
      }
    }
  }

  test("Empty Intersection") {
    forAll { i: Interval =>
      forAll(
        Gen
          .oneOf(
            distinctZipOpt(instantBeforeInterval(i), instantBeforeInterval(i)),
            distinctZipOpt(instantAfterInterval(i), instantAfterInterval(i))
          )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(other => i.intersection(other).isEmpty)
        )
      }
    }
  }

  test("Diff Interval") {
    forAll { i: Interval =>
      forAll(
        // At least one end within Interval i
        distinctZip(instantWithSpecialInterval(i), instantInInterval(i, includeStart = false))
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          val diff = i.diff(other)
          assert(diff.nonEmpty)
          assert(diff.forall(i.contains))
          assert(diff.forall(other.abuts))
          assert(
            diff
              .foldLeft(other.some)((a, b) => a.flatMap(_.join(b)))
              .flatMap(_.intersection(i)) === i.some
          )
        }
      }
    }
  }

  test("Unmodified Diff Interval") {
    forAll { i: Interval =>
      forAll(
        Gen
          .oneOf(
            distinctZipOpt(instantBeforeInterval(i), instantBeforeInterval(i)),
            distinctZipOpt(instantAfterInterval(i), instantAfterInterval(i))
          )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(other => i.diff(other) === List(i))
        )
      }
    }
  }

  test("Empty Diff Interval") {
    forAll { i: Interval =>
      forAll(
        distinctZipOpt(instantBeforeInterval(i), instantAfterInterval(i))
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(other => i.diff(other).isEmpty)
        )
      }
    }
  }

  test("Diff Schedule") {
    forAll(
      (for {
        i <- arbitrary[Interval]
        s <- arbitrary[Schedule]
      } yield (i, s)).suchThat {
        // Schedule that overlaps the interval but does not cover it completely.
        case (i, s) => s.restrictTo(i).nonEmpty && !s.covers(i)
      }
    ) {
      case (i, s) =>
        val diff = i.diff(s)
        assert(diff.intervals.forall(i.contains)) // Original interval contains all results.
        val restrictedSchedule = s.restrictTo(i)

        // Next val contains all intervals from diff plus all intervals from schedule limited to original interval.
        val intervals = (diff.intervals ++ restrictedSchedule.intervals).sorted
        assert(intervals.zip(intervals.tail).forall { case (a, b) => a.abuts(b) })
        assertEquals(restrictedSchedule.union(diff), Schedule(i))
    }
  }

  test("Unmodified Diff Schedule") {
    forAll { (i: Interval, s: Schedule) =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } {
        val disjointSchedule = s.restrictTo(before).union(s.restrictTo(after))
        assertEquals(i.diff(disjointSchedule), Schedule(i))
      }
    }
  }

  test("Empty Diff Schedule") {
    forAll { (i: Interval, s: Schedule) =>
      forAll(
        distinctZipOpt(instantBeforeInterval(i), instantAfterInterval(i))
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall { coveringInterval =>
              val coveringSchedule = s.union(coveringInterval)
              i.diff(coveringSchedule).isEmpty
            }
        )
      }
    }
  }

  test("ToFullDays") {
    forAll { (i: Interval, z: ZoneId, t: LocalTime) =>
      val allDay = i.toFullDays(z, t)
      assert(allDay.contains(i))
      // We can't comparte LocalTimes directly since the LocalTime may not exist at
      // the given day if there was a DST transition.
      val start  = allDay.start.atZone(z)
      assertEquals(start, start.`with`(t))
      val end    = allDay.end.atZone(z)
      assertEquals(end, end.`with`(t))
      assert(allDay.diff(i).forall(_.duration < Duration.ofDays(1)))
    }
  }
}

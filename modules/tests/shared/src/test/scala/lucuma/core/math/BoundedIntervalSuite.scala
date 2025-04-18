// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.core.arb.ArbTime
import lucuma.core.math.BoundedInterval
import lucuma.core.math.arb.ArbInterval
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import lucuma.core.optics.laws.discipline.WedgeTests
import lucuma.core.syntax.time.*
import lucuma.core.tests.ScalaCheckFlaky
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*
import spire.math.Interval

import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneId

class BoundedIntervalSuite  extends munit.DisciplineSuite with IntervalGens {
  import ArbInterval.given
  import ArbTime.*

  test("fromInterval") {
    forAll { (a: Int, b: Int) =>
      assertEquals(BoundedInterval.fromInterval(Interval.closed(a, b)).isDefined, a <= b)
      assertEquals(BoundedInterval.fromInterval(Interval.open(a, b)).isDefined, a < b)
      assertEquals(BoundedInterval.fromInterval(Interval.openLower(a, b)).isDefined, a < b)
      assertEquals(BoundedInterval.fromInterval(Interval.openUpper(a, b)).isDefined, a < b)
      assertEquals(BoundedInterval.fromInterval(Interval.above(b)).isDefined, false)
      assertEquals(BoundedInterval.fromInterval(Interval.atOrAbove(b)).isDefined, false)
      assertEquals(BoundedInterval.fromInterval(Interval.below(a)).isDefined, false)
      assertEquals(BoundedInterval.fromInterval(Interval.atOrBelow(a)).isDefined, false)
      assertEquals(BoundedInterval.fromInterval(Interval.empty[Int]).isDefined, false)
      assertEquals(BoundedInterval.fromInterval(Interval.all[Int]).isDefined, false)
    }
  }

  test("closed") {
    forAll { (a: Int, b: Int) =>
      assertEquals(BoundedInterval.closed(a, b).isDefined, a <= b)
    }
  }

  test("open") {
    forAll { (a: Int, b: Int) =>
      assertEquals(BoundedInterval.open(a, b).isDefined, a < b)
    }
  }


  test("openLower") {
    forAll { (a: Int, b: Int) =>
      assertEquals(BoundedInterval.openLower(a, b).isDefined, a < b)
    }
  }

  test("openUpper") {
    forAll { (a: Int, b: Int) =>
      assertEquals(BoundedInterval.openUpper(a, b).isDefined, a < b)
    }
  }

  test("abuts") {
    forAll { (i: BoundedInterval[Instant]) =>
      forAll(
        Gen.oneOf(
          instantBeforeInterval(i).map(
            _.map(s => BoundedInterval.unsafeOpenUpper(s, i.lower))
          ),
          instantAfterInterval(i)
            .map(_.map(e => BoundedInterval.unsafeOpenUpper(i.upper, e)))
        )
      ) { i2Opt =>
        assert(i2Opt.forall(i.abuts))
      }
    }
  }

  test("not abuts") {
    forAll { (i: BoundedInterval[Instant]) =>
      forAll(
        arbitrary[BoundedInterval[Instant]]
          .suchThat(i2 => i2.upper =!= i.lower)
          .suchThat(i2 => i2.lower =!= i.upper)
      ) { (i2: BoundedInterval[Instant]) =>
        assert(!i.abuts(i2))
      }
    }
  }

  test("join") {
    forAll { (i: BoundedInterval[Instant]) =>
      forAll(
        distinctZip(instantUntilEndOfInterval(i), instantFromStartOfInterval(i))
      ) { instants =>
        BoundedInterval.openUpperFromTuple[Instant].getOption(instants).foreach { other =>
          val join = i.join(other)
          assertEquals(join.map(_.lower), List(i.lower, other.lower).min.some)
          assertEquals(join.map(_.upper), List(i.upper, other.upper).max.some)
        }
      }
    }
  }

  test("empty join") {
    forAll { (i: BoundedInterval[Instant]) =>
      forAll(
        Gen
          .oneOf(
            distinctZipOpt(instantBeforeInterval(i), instantBeforeInterval(i)),
            distinctZipOpt(
              instantAfterInterval(i),
              instantAfterInterval(i)
            )
          )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(BoundedInterval.openUpperFromTuple[Instant].getOption)
            .forall(other => i.join(other).isEmpty)
        )
      }
    }
  }

  test("ToFullDays".tag(ScalaCheckFlaky)) {
    forAll { (i: BoundedInterval[Instant], z: ZoneId, t: LocalTime) =>
      val allDay = i.toFullDays(z, t)
      assert(i.isSubsetOf(allDay))
      // We can't comparte LocalTimes directly since the LocalTime may not exist at
      // the given day if there was a DST transition.
      val start  = allDay.lower.atZone(z)
      assertEquals(start, start.`with`(t))
      val end    = allDay.upper.atZone(z)
      assertEquals(end, end.`with`(t))
      assert((allDay -- i).forall(_.asInstanceOf[BoundedInterval[Instant]].duration < Duration.ofDays(1)))
    }
  }

  // Optics
  checkAll(
    "BoundedInterval.openUpperFromTuple",
    ValidSplitEpiTests(BoundedInterval.openUpperFromTuple[Int]).validSplitEpi
  )

  checkAll(
    "BoundedInterval.closedFromTuple",
    WedgeTests(BoundedInterval.closedFromTuple[Int]).wedge
  )
}

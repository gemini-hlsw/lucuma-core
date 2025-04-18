// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.order.*
import lucuma.core.arb.ArbTime.given
import lucuma.core.optics.laws.discipline.*
import lucuma.core.util.arb.ArbTimeSpan.given
import lucuma.core.util.arb.ArbTimestamp
import lucuma.core.util.arb.ArbTimestamp.given
import monocle.law.discipline.PrismTests
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.ZonedDateTime

class TimestampSuite extends DisciplineSuite {

  // Laws
  checkAll("Timestamp", OrderTests[Timestamp].order)
  checkAll("Timestamp.FromString", FormatTests(Timestamp.FromString).formatWith(ArbTimestamp.genTimestampString))
  checkAll("Timestamp.FromInstant", PrismTests(Timestamp.FromInstant))
  checkAll("Timestamp.FromLocalDateTime", PrismTests(Timestamp.FromLocalDateTime))

  test("Construction should truncate Instant nanoseconds to microseconds") {
    forAll { (i: Timestamp) =>
      i.toInstant.getNano % 1000L == 0
    }
  }

  test("Out of range dates are rejected") {
    implicit val arbInt: Arbitrary[Int] =
      Arbitrary {
        Gen.frequency((1, Gen.choose(-999999999, 999999999)), (1, Gen.choose(-4712, 294275)))
      }

    forAll { (y: Int) =>
      val i = ZonedDateTime.of(y, 1, 1, 0, 0, 0, 0, UTC).toInstant
      val t = Timestamp.fromInstant(i)

      assert(
        t.isEmpty == (i.isBefore(Timestamp.Min.toInstant) || i.isAfter(Timestamp.Max.toInstant))
      )
    }
  }

  test("Construction at microsecond precision only") {
    val inst = ZonedDateTime.of(2022, 8, 29, 12, 0, 0, 1, UTC).toInstant
    assertEquals(Option.empty[Timestamp], Timestamp.fromInstant(inst))
  }

  test("Parse options") {
    val ts = List(
      Timestamp.parse("1863-07-03 03:00:00"),
      Timestamp.parse("1863-07-03T03:00:00Z"),
      Timestamp.parse("1863-07-03 03:00:00.0"),
      Timestamp.parse("1863-07-03T03:00:00.0Z"),
      Timestamp.parse("1863-07-03 03:00:00.00"),
      Timestamp.parse("1863-07-03T03:00:00.00Z"),
      Timestamp.parse("1863-07-03 03:00:00.000"),
      Timestamp.parse("1863-07-03T03:00:00.000Z"),
      Timestamp.parse("1863-07-03 03:00:00.0000"),
      Timestamp.parse("1863-07-03T03:00:00.0000Z"),
      Timestamp.parse("1863-07-03 03:00:00.00000"),
      Timestamp.parse("1863-07-03T03:00:00.00000Z"),
      Timestamp.parse("1863-07-03 03:00:00.000000"),
      Timestamp.parse("1863-07-03T03:00:00.000000Z")
    )
    val expected = Instant.parse("1863-07-03T03:00:00Z")

    assert(ts.forall(_.toOption.map(_.toInstant) == Some(expected)))
  }

  test("Parse sub-microsecond fails") {
    val s = "1863-07-03 03:00:00.0000000"
    assertEquals(s"Could not parse as a Timestamp: $s".asLeft[Timestamp], Timestamp.parse(s))
  }

  property("Round-trip parse / format") {
    forAll { (t0: Timestamp) =>
      assertEquals(t0.some, Timestamp.parse(t0.format).toOption)
    }
  }

  property("intervalUntil") {
    forAll { (t0: Timestamp, t1: Timestamp) =>
      val ti = t0.intervalUntil(t1)
      assert(t0 < t1 == ti.nonEmpty)
    }
  }

  property("boundedAdd") {
    forAll: (t: Timestamp, ts: TimeSpan) =>
      assertEquals(t +| ts, t.plusMicrosOption(ts.toMicroseconds).getOrElse(Timestamp.Max))
  }

  property("boundedSubtract") {
    forAll: (t: Timestamp, ts: TimeSpan) =>
      assertEquals(t -| ts, t.plusMicrosOption(- ts.toMicroseconds).getOrElse(Timestamp.Min))
  }
}

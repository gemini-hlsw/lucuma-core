// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.uti
package util

import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbTimestamp._
import lucuma.core.instances.time._
import lucuma.core.math.arb.ArbTime._
import lucuma.core.math.laws.discipline._

import cats.kernel.laws.discipline._

import java.time.ZonedDateTime
import java.time.ZoneOffset.UTC

import org.scalacheck.Prop._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import munit._

final class TimestampSpec extends FunSuite with DisciplineSuite with ScalaCheckSuite {

  // Laws
  checkAll("Timestamp", OrderTests[Timestamp].order)
  checkAll("Timestamp.instant", FormatTests(Timestamp.instant).format)

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

}

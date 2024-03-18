// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc.solver

import cats.Eq
import cats.laws.discipline.InvariantSemigroupalTests
import cats.syntax.all.*
import lucuma.core.arb.ArbTime
import lucuma.core.syntax.time.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Duration
import java.time.Instant

final class SampleRounderSuite extends munit.DisciplineSuite {
  import ArbTime.given

  implicit def arbRounder[R, A](implicit r: SampleRounder[R, A]): Arbitrary[SampleRounder[R, A]] =
    Arbitrary(Gen.const(r))

  implicit def eqRounder[R, A: Eq: Arbitrary]: Eq[SampleRounder[R, A]] =
    Eq.instance { (rounderA, rounderB) =>
      val prop =
        Prop.forAll { (i0: Instant, i1: Instant, i2: Instant, v0: A, v1: A) =>
          val i = List(i0, i1, i2).sorted
          rounderA.round(i(0), v0, i(2), v1, i(1)) === rounderB.round(i(0), v0, i(2), v1, i(1))
        }
      prop(Gen.Parameters.default).success
    }

  // Laws
  checkAll("InvariantSemigroupal (Closest)",
           InvariantSemigroupalTests[SampleRounder[RoundStrategy.Closest, *]]
             .invariantSemigroupal[Int, Int, Int]
  )
  checkAll(
    "InvariantSemigroupal (LinearInterpolating)",
    InvariantSemigroupalTests[SampleRounder[RoundStrategy.LinearInterpolating, *]]
      .invariantSemigroupal[Int, Int, Int]
  )

  test("SampleRounder[Closest, Instant]") {
    val rounder = implicitly[SampleRounder[RoundStrategy.Closest, Instant]]
    forAll { (i0: Instant, i1: Instant, i2: Instant) =>
      val i        = List(i0, i1, i2).sorted
      val rounded  = rounder.round(i(0), i(0), i(2), i(2), i(1))
      val midpoint = i(0) + Duration.between(i(0), i(2)) / 2
      if (i(1) < midpoint)
        assert(rounded === i(0).some)
      else
        assert(rounded === i(2).some)
    }
  }

  test("SampleRounder[Closest, LinearInterpolating]") {
    val rounder = implicitly[SampleRounder[RoundStrategy.LinearInterpolating, Long]]
      .imap(Instant.ofEpochMilli)(_.toEpochMilli)
    forAll { (i0: Instant, i1: Instant, i2: Instant) =>
      val i       = List(i0, i1, i2).sorted
      val rounded = rounder.round(i(0), i(0), i(2), i(2), i(1))
      assert(
        rounded.exists(a => scala.math.abs(a.toEpochMilli - i(1).toEpochMilli) <= 1)
      ) // Since we are rounding to millis, we lose precision
    }
  }
}

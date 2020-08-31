// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc.solver

import cats.tests.CatsSuite
import cats.Eq
import cats.Eval
import java.time.Duration
import scala.collection.immutable.TreeMap
import lucuma.core.math.skycalc.solver.Samples.Bracket
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import lucuma.core.math.arb._
import cats.laws.discipline.FunctorTests
import cats.laws.discipline.MonoidKTests
import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline.IsoTests
import io.chrisdavenport.cats.time._

final class SamplesSpec extends CatsSuite {
  import ArbInterval._
  import ArbSamples._
  import ArbTime._
  import ArbEval._

  // Laws
  checkAll("Eq", EqTests[Samples[Int]].eqv)
  checkAll("Functor", FunctorTests[Samples].functor[Int, Int, Int])
  checkAll("MonoidK", MonoidKTests[Samples].monoidK[Int])

  // The explicit cast to Map seems to be needed for implicit resolution.
  implicit def treeMapEq[K: Eq, V: Eq]: Eq[TreeMap[K, V]] = Eq.by(_.toMap)

  // Optics
  checkAll("data", IsoTests(Samples.data[Int]))

  test("Fixed Rate Instants") {
    forAll { interval: Interval =>
      forAll(rateForInterval(interval)) { duration: Duration =>
        val fixedRateSamples = Samples.atFixedRate(interval, duration)(_ => Eval.now(()))
        val instants         = fixedRateSamples.toMap.keys.toList
        assert(instants.head === interval.start)
        val last             = instants.last
        val maxSample        = interval.end.plus(duration)
        assert(last >= interval.end)
        assert(last < maxSample)
        val intervalSamples  = interval.duration.toNanos / duration.toNanos + 1
        assert(instants.length >= intervalSamples)
        assert(instants.length <= intervalSamples + 1)
      }
    }
  }

  test("Bracket") {
    forAll(arbitrary[Samples[Unit]].suchThat(_.interval.isDefined)) { samples =>
      val interval = samples.interval.get
      forAll(instantInInterval(interval, includeEnd = true)) { i =>
        samples.bracket(i) match {
          case Bracket(Some((i0, _)), Some((i1, _)), Some((i2, _))) =>
            assert(i0 >= interval.start)
            assert(i0 < i1)
            assert(i1 === i)
            assert(i1 < i2)
            assert(i2 <= interval.end)
          case Bracket(Some((i0, _)), None, Some((i2, _)))          =>
            assert(i0 >= interval.start)
            assert(i0 < i)
            assert(i < i2)
            assert(i2 <= interval.end)
          case Bracket(None, Some((i1, _)), Some((i2, _)))          =>
            assert(i1 === interval.start)
            assert(i1 === i)
            assert(i1 < i2)
            assert(i2 <= interval.end)
          case Bracket(Some((i0, _)), Some((i1, _)), None)          =>
            assert(i0 >= interval.start)
            assert(i1 === i)
            assert(i0 < i1)
            assert(i1 === interval.end)
          case Bracket(None, Some((_, _)), None)                    =>
            fail(s"$i returned a single-element bracket")
          case Bracket(None, None, _)                               =>
            fail(s"$i returned an out-of-bounds bracket")
          case Bracket(_, None, None)                               =>
            fail(s"$i returned an out-of-bounds bracket")
        }
      }
      forAll(instantOutsideInterval(interval, includeEnd = false)) {
        _.foreach { i =>
          samples.bracket(i) match {
            case Bracket(left, None, right) =>
              assert(left.isEmpty || right.isEmpty)
            case _                          =>
              fail(s"$i returned an in-bounds bracket")
          }
        }
      }
    }
  }
}

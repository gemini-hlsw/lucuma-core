// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.Duration
import java.time.Instant
import org.scalacheck.Gen._
import gsp.math.arb._
import io.chrisdavenport.cats.time._

final class CalculatorSpec extends CatsSuite {
  import ArbInterval._

  case class TestCalculator(interval: Interval, rate: Duration) extends FixedRateCalculator[Unit] {
    override val result: Instant => Unit = _ => ()
  }

  test("Fixed Rate Instants") {
    forAll { interval: Interval =>
      forAll(rateForInterval(interval)) { duration: Duration =>
        val intervalTargetCalculator = TestCalculator(interval, duration)
        val instants                 = intervalTargetCalculator.instants
        assert(instants.head === interval.start)
        val last                     = instants.last
        assert(last >= interval.end)
        assert(last < interval.end.plus(duration))
        forAll(instantInInterval(interval, includeEnd = true)) { instant: Instant =>
          val idx = intervalTargetCalculator.toIndex(instant).toLong
          assert(idx >= 0 && idx < instants.length)
          assert(instants.get(idx).exists(_ <= instant))
          assert(instants.get(idx + 1).forall(_ > instant))
        }
      }
    }
  }
}

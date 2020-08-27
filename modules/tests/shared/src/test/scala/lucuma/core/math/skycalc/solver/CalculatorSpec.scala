// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.Duration
import org.scalacheck.Gen._
import gsp.math.arb._
import io.chrisdavenport.cats.time._
import cats.Eval
import gsp.math.skycalc.solver.Samples.Bracket

final class CalculatorSpec extends CatsSuite {
  import ArbInterval._

  test("Fixed Rate Instants") {
    forAll { interval: Interval =>
      forAll(rateForInterval(interval)) { duration: Duration =>
        val intervalTargetCalculator = Samples.atFixedRate(interval, duration)(_ => Eval.now(()))
        val instants                 = intervalTargetCalculator.toMap.keys
        assert(instants.head === interval.start)
        val last                     = instants.last
        assert(last >= interval.end)
        assert(last < interval.end.plus(duration))
        forAll(instantInInterval(interval, includeEnd = true)) { i =>
          intervalTargetCalculator.bracket(i) match {
            case Bracket(Some((i0, _)), Some((i1, _)), Some((i2, _))) =>
              assert(i0 >= interval.start)
              assert(i0 < i1)
              assert(i1 < i2)
              assert(i2 <= interval.end)
            case Bracket(None, None, None) => fail(s"Couldn't find $i")
            case _ => succeed
          }
        }
      }
    }
  }
}

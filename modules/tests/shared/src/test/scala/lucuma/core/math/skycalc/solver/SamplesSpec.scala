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

final class SamplesSpec extends CatsSuite {
  import ArbInterval._

  test("Fixed Rate Instants") {
    forAll { interval: Interval =>
      forAll(rateForInterval(interval)) { duration: Duration =>
        val fixedRateSamples = Samples.atFixedRate(interval, duration)(_ => Eval.now(()))
        val instants         = fixedRateSamples.toMap.keys
        assert(instants.head === interval.start)
        val last             = instants.last
        val maxSample        = interval.end.plus(duration)
        assert(last >= interval.end)
        assert(last < maxSample)
        forAll(instantInInterval(interval, includeEnd = true)) { i =>
          fixedRateSamples.bracket(i) match {
            case Bracket(Some((i0, _)), Some((i1, _)), Some((i2, _))) =>
              assert(i0 >= interval.start)
              assert(i0 < i1)
              assert(i1 === i)
              assert(i1 < i2)
              assert(i2 < maxSample)
            case Bracket(Some((i0, _)), None, Some((i2, _)))          =>
              assert(i0 >= interval.start)
              assert(i0 < i)
              assert(i < i2)
              assert(i2 < maxSample)
            case Bracket(None, Some((i1, _)), Some((i2, _)))          =>
              assert(i1 === interval.start)
              assert(i1 === i)
              assert(i1 < i2)
              assert(i2 < maxSample)
            case Bracket(None, Some((_, _)), None)                    =>
              fail(s"$i returned a single-element bracket")
            case Bracket(Some((_, _)), Some((_, _)), None)            =>
              fail(s"$i returned a last-element bracket")
            case Bracket(None, None, _)                               =>
              fail(s"$i returned an out-of-bounds bracket")
            case Bracket(_, None, None)                               =>
              fail(s"$i returned an out-of-bounds bracket")
          }
        }
      }
    }
  }

  // single
  // fromtree
  // emtpy?
  // make a constant constructor? (eg: for coordinates/place?)

  // functor
  // monoid

}

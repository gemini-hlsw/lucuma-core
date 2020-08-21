// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import gsp.math.arb.ArbDeclinationAngularVelocity._
import gsp.math.laws.discipline.SplitMonoTests
import monocle.law.discipline._
import spire.math.Rational

final class DeclinationAngularVelocitySpec extends CatsSuite {

  // Laws
  checkAll("Order[DeclinationAngularVelocity]", OrderTests[DeclinationAngularVelocity].order)
  checkAll("Monoid[DeclinationAngularVelocity]", MonoidTests[DeclinationAngularVelocity].monoid)
  checkAll("DeclinationAngularVelocity.microarcsecondsPerYear",
           IsoTests(DeclinationAngularVelocity.microarcsecondsPerYear)
  )
  checkAll("DeclinationAngularVelocity.milliarcsecondsPerYear",
           SplitMonoTests(DeclinationAngularVelocity.milliarcsecondsPerYear).splitMono
  )

  test("fromDouble") {
    // we get proper velocity in mas/y from simbad
    val masy = 19.34 // dec mas/y for vega
    assert(
      DeclinationAngularVelocity.milliarcsecondsPerYear.reverseGet(masy).masy.value ==
        Rational(1934, 100)
    )
  }
}

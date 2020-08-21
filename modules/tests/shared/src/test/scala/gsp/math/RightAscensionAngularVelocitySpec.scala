// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import gsp.math.arb.ArbRightAscensionAngularVelocity._
import gsp.math.laws.discipline.SplitMonoTests
import monocle.law.discipline._
import spire.math.Rational

final class RightAscensionAngularVelocitySpec extends CatsSuite {

  // Laws
  checkAll("Order[RightAscensionAngularVelocity]", OrderTests[RightAscensionAngularVelocity].order)
  checkAll("Monoid[RightAscensionAngularVelocity]",
           MonoidTests[RightAscensionAngularVelocity].monoid
  )
  checkAll("RightAscensionAngularVelocity.microarcseconds",
           IsoTests(RightAscensionAngularVelocity.microarcsecondsPerYear)
  )
  checkAll("RightAscensionAngularVelocity.milliarcsecondsPerYear",
           SplitMonoTests(RightAscensionAngularVelocity.milliarcsecondsPerYear).splitMono
  )

  test("fromDouble") {
    // we get proper velocity in mas/y from simbad
    val masy = -43.67 // ra mas/y for veg  a
    assert(
      RightAscensionAngularVelocity.milliarcsecondsPerYear.reverseGet(masy).masy.value ===
        Rational(-4367, 100)
    )
  }
}

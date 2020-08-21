// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import gsp.math.arb.ArbParallax._
import gsp.math.laws.discipline.SplitMonoTests
import monocle.law.discipline._
import spire.math.Rational

final class ParallaxSpec extends CatsSuite {

  // Laws
  checkAll("Order[Parallax]", OrderTests[Parallax].order)
  checkAll("Monoid[Parallax]", IsoTests(Parallax.microarcseconds))
  checkAll("Parallax.microarcseconds", IsoTests(Parallax.microarcseconds))
  checkAll("Parallax.signedMicroarcseconds", SplitMonoTests(Parallax.milliarcseconds).splitMono)

  test("fromDouble") {
    // we get parallax in mas from simbad
    val mas = 8.09 // parallax for vega
    assert(Parallax.milliarcseconds.reverseGet(mas).mas.value == Rational(809, 100))
  }
}

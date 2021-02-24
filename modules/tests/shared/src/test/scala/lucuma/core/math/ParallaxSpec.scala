// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import lucuma.core.math.arb.ArbParallax._
import lucuma.core.optics.laws.discipline._
import spire.math.Rational

final class ParallaxSpec extends CatsSuite {

  // Laws
  checkAll("Order[Parallax]", OrderTests[Parallax].order)
  checkAll("Parallax.microarcseconds", SplitMonoTests(Parallax.microarcseconds).splitMono)
  checkAll("Parallax.milliarcseconds", SplitMonoTests(Parallax.milliarcseconds).splitMono)

  test("fromDouble") {
    // we get parallax in mas from simbad
    val mas = 8.09 // parallax for vega
    assert(Parallax.milliarcseconds.reverseGet(mas).mas.value === Rational(809, 100))
  }

  test("fromMicroarcseconds") {
    def toμas(deg: Int, μas: Int = 0): Long =
      deg.toLong * Angle.µasPerDegree + μas.toLong
    def from(deg:  Int, μas: Int = 0)       =
      Parallax.fromMicroarcseconds(toμas(deg, μas)).μas.value.value
    from(12) shouldEqual toμas(12)
    from(-12) shouldEqual toμas(12)
    from(180) shouldEqual toμas(180)
    from(-180) shouldEqual toμas(180)
    from(180, 1) shouldEqual toμas(180, -1)
    from(-180, 1) shouldEqual toμas(180, -1)
    from(180, -1) shouldEqual toμas(180, -1)
    from(-180, -1) shouldEqual toμas(180, -1)
    from(360) shouldEqual 0L
    from(-360) shouldEqual 0L
    from(360, 1) shouldEqual toμas(0, 1)
    from(-360, 1) shouldEqual toμas(0, 1)
    from(360, -1) shouldEqual toμas(0, 1)
    from(-360, -1) shouldEqual toμas(0, 1)
    from(365) shouldEqual toμas(5)
    from(-365) shouldEqual toμas(5)
    from(541) shouldEqual toμas(179)
    from(-541) shouldEqual toμas(179)
    from(719) shouldEqual toμas(1)
    from(-719) shouldEqual toμas(1)
  }
}

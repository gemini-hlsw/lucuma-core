// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline.*
import lucuma.core.math.arb.ArbParallax.given
import lucuma.core.optics.laws.discipline.*
import spire.math.Rational

final class ParallaxSuite extends munit.DisciplineSuite {

  // Laws
  checkAll("Order[Parallax]", OrderTests[Parallax].order)
  checkAll("Parallax.microarcseconds", SplitMonoTests(Parallax.microarcseconds).splitMono)
  checkAll("Parallax.milliarcseconds", SplitMonoTests(Parallax.milliarcseconds).splitMono)

  test("fromDouble") {
    // we get parallax in mas from simbad
    val mas = 8.09 // parallax for vega
    assertEquals(Parallax.milliarcseconds.reverseGet(mas).mas.value, Rational(809, 100))
  }

  test("fromMicroarcseconds") {
    def toμas(deg: Int, μas: Int = 0): Long =
      deg.toLong * Angle.µasPerDegree + μas.toLong
    def from(deg:  Int, μas: Int = 0)       =
      Parallax.fromMicroarcseconds(toμas(deg, μas)).μas.value.value
    assertEquals(from(12),  toμas(12))
    assertEquals(from(-12),  toμas(12))
    assertEquals(from(180),  toμas(180))
    assertEquals(from(-180),  toμas(180))
    assertEquals(from(180, 1),  toμas(180, -1))
    assertEquals(from(-180, 1),  toμas(180, -1))
    assertEquals(from(180, -1),  toμas(180, -1))
    assertEquals(from(-180, -1),  toμas(180, -1))
    assertEquals(from(360),  0L)
    assertEquals(from(-360),  0L)
    assertEquals(from(360, 1),  toμas(0, 1))
    assertEquals(from(-360, 1),  toμas(0, 1))
    assertEquals(from(360, -1),  toμas(0, 1))
    assertEquals(from(-360, -1),  toμas(0, 1))
    assertEquals(from(365),  toμas(5))
    assertEquals(from(-365),  toμas(5))
    assertEquals(from(541),  toμas(179))
    assertEquals(from(-541),  toμas(179))
    assertEquals(from(719),  toμas(1))
    assertEquals(from(-719),  toμas(1))
  }
}

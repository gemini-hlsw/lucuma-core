// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.all.*
import lucuma.core.util.Enumerated
import munit.DisciplineSuite

final class GnirsDisperserOrderSuite extends DisciplineSuite {

  // Every value used to throw on construction: orders One and Two built a Wavelength from
  // zero for their (undefined) delta wavelength, so merely touching the enum failed.
  test("every value is constructible") {
    assertEquals(Enumerated[GnirsDisperserOrder].all.size, 9)
    Enumerated[GnirsDisperserOrder].all.foreach: o =>
      assert(o.count >= 1 && o.count <= 8, s"unexpected order count for ${o.tag}")
      assert(o.minWavelength < o.maxWavelength, s"empty range for ${o.tag}")
  }

  test("the delta wavelength is defined exactly for the cross-dispersed orders") {
    Enumerated[GnirsDisperserOrder].all.foreach: o =>
      assertEquals(o.deltaWavelength.isDefined, o.crossDispersed, s"delta/XD mismatch for ${o.tag}")
    assertEquals(GnirsDisperserOrder.One.deltaWavelength, None)
    assertEquals(GnirsDisperserOrder.Two.deltaWavelength, None)
    assertEquals(
      GnirsDisperserOrder.Three.deltaWavelength.map(_.toPicometers.value.value),
      Some(647)
    )
  }

  test("order 4 appears twice, cross-dispersed and not") {
    val fours = Enumerated[GnirsDisperserOrder].all.filter(_.count == 4)
    assertEquals(fours.map(_.tag), List("FourXD", "Four"))
  }

}

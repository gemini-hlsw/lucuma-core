// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import lucuma.core.math.BrightnessValue
import munit.FunSuite

class GaiaPhotometrySuite extends FunSuite:

  private def bv(d: Double): BrightnessValue = BrightnessValue.unsafeFrom(BigDecimal(d))

  test("G - R matches the DR3 polynomial at sample colours") {
    assertEqualsDouble(GaiaPhotometry.gMinusR(0.0), -0.02275, 1e-9)
    // -0.02275 + 0.3961 - 0.1243 - 0.01396 + 0.003775
    assertEqualsDouble(GaiaPhotometry.gMinusR(1.0), 0.238865, 1e-9)
    // -0.02275 + 0.7922 - 0.4972 - 0.11168 + 0.0604
    assertEqualsDouble(GaiaPhotometry.gMinusR(2.0), 0.22097, 1e-9)
  }

  test("R is G minus the colour term inside the validity range") {
    // BP - RP = 1.0
    val r = GaiaPhotometry.johnsonCousinsR(bv(14.5), bv(15.2), bv(14.2))
    assertEqualsDouble(r.map(_.value.value.toDouble).getOrElse(Double.NaN), 14.5 - 0.238865, 1e-6)
  }

  test("R is undefined outside the validity range") {
    // Blue star, BP - RP < 0
    assertEquals(GaiaPhotometry.johnsonCousinsR(bv(10.0), bv(9.8), bv(10.1)), None)
    // Very red star, BP - RP > 4
    assertEquals(GaiaPhotometry.johnsonCousinsR(bv(10.0), bv(14.5), bv(10.0)), None)
  }

  test("G - R bounds bracket the polynomial over the validity range") {
    val (min, max) = GaiaPhotometry.GMinusRBounds
    assert(min <= GaiaPhotometry.gMinusR(0.0))
    assert(min <= GaiaPhotometry.gMinusR(4.0))
    assert(max >= GaiaPhotometry.gMinusR(1.5))
    // The polynomial peaks near BP - RP = 1.5 and dips at the red end
    assertEqualsDouble(max, 0.264, 1e-3)
    assertEqualsDouble(min, -0.354, 1e-3)
  }

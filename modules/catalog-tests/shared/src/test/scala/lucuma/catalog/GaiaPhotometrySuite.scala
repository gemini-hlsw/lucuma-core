// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import munit.FunSuite

import scala.collection.immutable.SortedMap

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

  test("G - R bounds bracket the polynomial over the validity range, padded outward") {
    val (min, max) = GaiaPhotometry.GMinusRBounds
    (0 to 400).map(_ / 100.0).foreach { colour =>
      assert(min < GaiaPhotometry.gMinusR(colour))
      assert(max > GaiaPhotometry.gMinusR(colour))
    }
    // The polynomial peaks at 0.2645 near BP - RP = 1.43 and dips to -0.3542 at the red end
    assertEqualsDouble(max, 0.27, 1e-9)
    assertEqualsDouble(min, -0.36, 1e-9)
  }

  test("estimatedR derives R from G, BP and RP") {
    val gaia = SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.5),
                                                Band.GaiaBP -> bv(15.2),
                                                Band.GaiaRP -> bv(14.2)
    )
    assertEqualsDouble(GaiaPhotometry.estimatedR(gaia).get.value.value.toDouble,
                       14.5 - 0.238865,
                       1e-6
    )
  }

  test("estimatedR needs all three Gaia bands") {
    assertEquals(GaiaPhotometry.estimatedR(SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.5))),
                 None
    )
    assertEquals(
      GaiaPhotometry.estimatedR(
        SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.5), Band.GaiaRP -> bv(14.2))
      ),
      None
    )
  }

  test("estimated brightnesses follow the DR3 polynomials at BP - RP = 1"):
    val gaia = SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.0),
                                                Band.GaiaBP -> bv(14.5),
                                                Band.GaiaRP -> bv(13.5)
    )
    val est  = GaiaPhotometry.estimatedBrightnesses(gaia)
    assertEquals(est.keySet, GaiaPhotometry.EstimatedBands)
    // G - V = -0.02704 + 0.01424 - 0.2156 + 0.01426
    assertEqualsDouble(est(Band.V).value.value.toDouble, 14.0 + 0.21414, 1e-3)
    // G - H = -0.1048 + 2.011 - 0.1758
    assertEqualsDouble(est(Band.H).value.value.toDouble, 14.0 - 1.7304, 1e-3)
    // G - r = -0.09837 + 0.08592 + 0.1907 - 0.1701 + 0.02263
    assertEqualsDouble(est(Band.SloanR).value.value.toDouble, 14.0 - 0.03078, 1e-3)

  test("bands whose colour range excludes the star are left out"):
    // BP - RP = 3.5: V (-0.5, 5.0) is in range, every other transformation is not
    val red  = SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.0),
                                               Band.GaiaBP -> bv(16.0),
                                               Band.GaiaRP -> bv(12.5)
    )
    assertEquals(GaiaPhotometry.estimatedBrightnesses(red).keySet, Set[Band](Band.V))
    // BP - RP = 0.1: Sloan g (0.3, 3.0) and i (0.5, 2.0) are out
    val blue = SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.0),
                                                Band.GaiaBP -> bv(14.0),
                                                Band.GaiaRP -> bv(13.9)
    )
    assertEquals(
      GaiaPhotometry.estimatedBrightnesses(blue).keySet,
      Set[Band](Band.V, Band.SloanR, Band.J, Band.H, Band.K)
    )

  test("no estimates without all three Gaia bands"):
    assertEquals(
      GaiaPhotometry.estimatedBrightnesses(SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.0))),
      SortedMap.empty[Band, BrightnessValue]
    )

  test("the validity range includes its edges, as in the ADQL query for R"):
    // BP - RP = 0.0
    assert(GaiaPhotometry.johnsonCousinsR(bv(14.0), bv(14.0), bv(14.0)).isDefined)
    // BP - RP = 4.0
    assert(GaiaPhotometry.johnsonCousinsR(bv(14.0), bv(17.0), bv(13.0)).isDefined)

  test("estimates are not rounded"):
    val gaia = SortedMap[Band, BrightnessValue](Band.Gaia -> bv(14.0),
                                                Band.GaiaBP -> bv(14.5),
                                                Band.GaiaRP -> bv(13.5)
    )
    // G - V = -0.02704 + 0.01424 - 0.2156 + 0.01426 = -0.21414
    assertEqualsDouble(
      GaiaPhotometry.estimatedBrightnesses(gaia)(Band.V).value.value.toDouble,
      14.21414,
      1e-9
    )

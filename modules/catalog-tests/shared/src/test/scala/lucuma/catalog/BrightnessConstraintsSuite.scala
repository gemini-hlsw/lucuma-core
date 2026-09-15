// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.kernel.laws.discipline.*
import lucuma.catalog.arb.all.given
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import munit.*

class BrightnessConstraintsSuite extends DisciplineSuite {

  // Laws
  checkAll("Eq[BrightnessConstraints]", EqTests[BrightnessConstraints].eqv)
  checkAll("Order[FaintnessConstraint]", OrderTests[FaintnessConstraint].order)
  checkAll("Order[SaturationConstraint]", OrderTests[SaturationConstraint].order)

  test("filter targets on band and faintness") {
    val bc = BrightnessConstraints(BandsList.GaiaBandsList,
                                   FaintnessConstraint(BrightnessValue.unsafeFrom(10.0)),
                                   None
    )
    assert(bc.contains(Band.Gaia, BrightnessValue.unsafeFrom(3.0)))
    // no matching band
    assert(!bc.contains(Band.R, BrightnessValue.unsafeFrom(3.0)))
    // Too faint
    assert(!bc.contains(Band.Gaia, BrightnessValue.unsafeFrom(12.0)))
  }

  test("filter targets on band, faintness and saturation") {
    val bc = BrightnessConstraints(BandsList.GaiaBandsList,
                                   FaintnessConstraint(BrightnessValue.unsafeFrom(10.0)),
                                   Some(SaturationConstraint(BrightnessValue.unsafeFrom(2)))
    )
    assert(bc.contains(Band.Gaia, BrightnessValue.unsafeFrom(3.0)))
    // no matching band
    assert(!bc.contains(Band.R, BrightnessValue.unsafeFrom(3.0)))
    // Too faint
    assert(!bc.contains(Band.Gaia, BrightnessValue.unsafeFrom(12.0)))
    // Saturated
    assert(!bc.contains(Band.Gaia, BrightnessValue.unsafeFrom(1.0)))
  }

  test("filter targets on the R band") {
    val bc = BrightnessConstraints(
      BandsList.RBandsList,
      FaintnessConstraint(BrightnessValue.unsafeFrom(15.0)),
      Some(SaturationConstraint(BrightnessValue.unsafeFrom(8.0)))
    )
    assert(bc.contains(Band.R, BrightnessValue.unsafeFrom(12.0)))
    // Gaia bands don't satisfy an R constraint
    assert(!bc.contains(Band.Gaia, BrightnessValue.unsafeFrom(12.0)))
    assert(!bc.contains(Band.R, BrightnessValue.unsafeFrom(15.5)))
    assert(!bc.contains(Band.R, BrightnessValue.unsafeFrom(7.5)))
  }

  test("union of band lists covers both") {
    val union = BandsList.GaiaBandsList ∪ BandsList.RBandsList
    assertEquals(union.bands.toSet, Set[Band](Band.R, Band.GaiaRP, Band.Gaia, Band.GaiaBP))
    assertEquals(BandsList.RBandsList ∪ BandsList.GaiaBandsList, union)
    assertEquals(union ∪ BandsList.GaiaBandsList, union)
    assertEquals(BandsList.GaiaBandsList ∪ BandsList.GaiaBandsList, BandsList.GaiaBandsList)
    assertEquals(BandsList.RBandsList ∪ BandsList.RBandsList, BandsList.RBandsList)
  }
}

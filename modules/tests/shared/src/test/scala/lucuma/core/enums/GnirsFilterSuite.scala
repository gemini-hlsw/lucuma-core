// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.math.Wavelength
import munit.DisciplineSuite

final class GnirsFilterSuite extends DisciplineSuite {

  private def acq(pm: Int): GnirsFilter =
    GnirsFilter.fromAcquisitionWavelength(Wavelength.unsafeFromIntPicometers(pm))

  private def checkAcq(pm: Int, expected: GnirsFilter): Unit =
    assertEquals(acq(pm), expected, s"acquisition filter at ${pm}pm")

  test("acquisition filter, wavelength within a filter's range") {
    checkAcq(1_030_000, GnirsFilter.Order6) // X, lower bound
    checkAcq(1_100_000, GnirsFilter.Order6)
    checkAcq(1_250_000, GnirsFilter.Order5) // J
    checkAcq(1_650_000, GnirsFilter.Order4) // H
    checkAcq(2_200_000, GnirsFilter.Order3) // K
  }

  test("acquisition filter, H2 wins over the Order3 range that contains it") {
    checkAcq(2_105_000, GnirsFilter.H2)
    checkAcq(2_120_000, GnirsFilter.H2)
    checkAcq(2_135_999, GnirsFilter.H2)
    checkAcq(2_136_000, GnirsFilter.Order3) // ranges are open above
  }

  test("acquisition filter, gaps between the order filters use the nearest broadband filter") {
    checkAcq(1_000_000, GnirsFilter.Order6) // below X
    checkAcq(1_400_000, GnirsFilter.Order5) // J/H gap, nearer J
    checkAcq(1_480_000, GnirsFilter.Order4) // J/H gap, nearer H
    checkAcq(1_800_000, GnirsFilter.Order4) // H upper bound is exclusive
    checkAcq(1_850_000, GnirsFilter.Order4) // H/K gap: the LC SXD optimal wavelength
    checkAcq(2_490_000, GnirsFilter.Order3) // K upper bound is exclusive
  }

  test("acquisition filter, thermal-IR science") {
    checkAcq(2_500_000, GnirsFilter.ThermalAcquisitionFilter) // the cutoff itself
    checkAcq(2_800_000, GnirsFilter.ThermalAcquisitionFilter) // L, lower bound
    checkAcq(3_300_000, GnirsFilter.ThermalAcquisitionFilter) // the PAH band
    checkAcq(3_500_000, GnirsFilter.ThermalAcquisitionFilter) // L, optimal
    checkAcq(4_300_000, GnirsFilter.ThermalAcquisitionFilter) // L/M gap
    checkAcq(4_800_000, GnirsFilter.ThermalAcquisitionFilter) // M, optimal
    checkAcq(5_000_000, GnirsFilter.ThermalAcquisitionFilter) // the reported failure
    checkAcq(6_000_000, GnirsFilter.ThermalAcquisitionFilter) // M upper bound is exclusive
  }

  test("acquisition filter selection is total over (and beyond) the GNIRS range") {
    (500_000 to 7_000_000 by 1_000).foreach: pm =>
      assert(GnirsFilter.AutoAcquisitionFilters.toList.contains(acq(pm)), s"no acquisition filter at ${pm}pm")
  }

  test("automatic acquisition selection never produces PAH") {
    assert(!GnirsFilter.AutoAcquisitionFilters.toList.contains(GnirsFilter.PAH))
    assert(GnirsFilter.SpectroscopyAcquisitionFilters.toList.contains(GnirsFilter.PAH))
  }

  test("imaging acquisition filters are the imaging filters, plus H2") {
    // Order is presentational, so compare membership.
    val (h2, imaging) = GnirsFilter.ImagingAcquisitionFilters.toList.partition(_ === GnirsFilter.H2)
    assertEquals(h2, List(GnirsFilter.H2))
    assertEquals(imaging.toSet, GnirsFilter.values.filter(_.imagingWidth.isDefined).toSet)
  }

  test("the J and K of an imaging acquisition are the photometric filters, not the orders") {
    val imaging = GnirsFilter.ImagingAcquisitionFilters.toList
    assert(imaging.contains(GnirsFilter.J) && imaging.contains(GnirsFilter.K))
    assert(!imaging.contains(GnirsFilter.Order5) && !imaging.contains(GnirsFilter.Order3))
    val spectroscopy = GnirsFilter.SpectroscopyAcquisitionFilters.toList
    assert(spectroscopy.contains(GnirsFilter.Order5) && spectroscopy.contains(GnirsFilter.Order3))
    assert(!spectroscopy.contains(GnirsFilter.J) && !spectroscopy.contains(GnirsFilter.K))
  }

  test("science filter") {
    assertEquals(
      GnirsFilter.fromSpectroscopyScienceWavelength(Wavelength.unsafeFromIntPicometers(5_000_000)),
      Right(GnirsFilter.Order1)
    )
    assert(
      GnirsFilter.fromSpectroscopyScienceWavelength(Wavelength.unsafeFromIntPicometers(2_600_000)).isLeft
    )
  }

}

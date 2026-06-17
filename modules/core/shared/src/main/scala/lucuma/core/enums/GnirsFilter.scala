// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.Order.given
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

import scala.collection.immutable.SortedMap

import ConvenienceOps.*

/**
 * Enumerated type for GNIRS Filter.
 * @group Enumerations (Generated)
 */
enum GnirsFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  // There are two J filters (ORDER5 and J-MK) and two K filters (ORDER3 and K-MK).  The "ORDER" filters are for spectroscopy and have wider
  // wavelength coverage, while the "MK" filters are for imaging and are matched to the bandpasses of the Maunakea photometric system.
  // The "ORDER" filters are physically large and cover the length of the slit, while the MK filters are small and only cover the inner circular region.
  // Thus for spectroscopic acquisitions we use the ORDER filters while image science observations use the MK filters. The one outlier
  // is the ORDER4 filter which has approximately the H-MK bandpass so it will be used for both spectroscopic acquisitions and imaging science.
  // Note that only the "ORDER" filters are valid for spectroscopy science. The other ones with spectroscopyRange defined (H2 and PAH)
  // can still be used for acquisition.
  //
  // ATTENTION: The optimal wavelength and spectroscopy range are duplicated in the DB view in the ODB. Modify it there too if it's changed here.
  val optimalWavelength: Option[Wavelength],
  val spectroscopyRange: Option[BoundedInterval[Wavelength]], // Range of the spectroscopy filters.
) derives Enumerated, Display:
  case CrossDispersed extends GnirsFilter("CrossDispersed", "XD", "Cross dispersed", none, none)
  case Order6 extends GnirsFilter("Order6", "X", "Order 6 (X)", 1_100_000.pm.some, (1_030_000, 1_175_400).pmRange.some)
  case Order5 extends GnirsFilter("Order5", "J", "Order 5 (J)", 1_270_000.pm.some, (1_175_400, 1_370_000).pmRange.some)
  case Order4 extends GnirsFilter("Order4", "H", "Order 4 (H: 1.645µm)", 1_645_000.pm.some, (1_490_000, 1_800_000).pmRange.some)
  // Declaration order matters for range match. Since H2 is completely contained in Order3, it needs to come before or it will never be selected.
  case H2 extends GnirsFilter("H2", "H2", "H2: 2.122µm", 2_122_000.pm.some, (2_105_000, 2_136_000).pmRange.some)
  case Order3 extends GnirsFilter("Order3", "K", "Order 3 (K)", 2_200_000.pm.some, (1_910_000, 2_490_000).pmRange.some)
  case Order2 extends GnirsFilter("Order2", "L", "Order 2 (L)", 3_500_000.pm.some, (2_800_000, 4_200_000).pmRange.some)
  case Order1 extends GnirsFilter("Order1", "M", "Order 1 (M)", 4_800_000.pm.some, (4_400_000, 6_000_000).pmRange.some)
  case HNd100x extends GnirsFilter("HNd100x", "H+ND100X", "H + ND100X", 1_640_000.pm.some, none)
  case H2Nd100x extends GnirsFilter("H2Nd100x", "H2+ND100X", "H2 + ND100X", 2_122_000.pm.some, none)
  case PAH extends GnirsFilter("PAH", "PAH", "PAH: 3.295µm", 3_295_000.pm.some, (3_266_000, 3_321_000).pmRange.some)
  case Y extends GnirsFilter("Y", "Y", "Y: 1.02µm", 1_020_000.pm.some, none)
  case J extends GnirsFilter("J", "J", "J: 1.25µm", 1_250_000.pm.some, none)
  case K extends GnirsFilter("K", "K", "K: 2.20µm", 2_200_000.pm.some, none)

  def centralWavelength: Wavelength =
    // The only case the filter optimalWavelength is none is for XD, where we fix to 1.65um.
    optimalWavelength.getOrElse(Wavelength.unsafeFromIntPicometers(1_650_000))

object GnirsFilter:
  private val SpectroscopyFilterTable: NonEmptyMap[GnirsFilter, BoundedInterval[Wavelength]] = 
    NonEmptyMap.fromMapUnsafe:
      SortedMap.from(values.flatMap(f => f.spectroscopyRange.map(w => (f, w))))

  /** Acquisition filter options. */
  val AcquisitionFilters: NonEmptySet[GnirsFilter] =
    NonEmptySet.of(Order6, Order5, Order4, H2, Order3, PAH)

  def fromSpectroscopyWavelength(wavelength: Wavelength, isAcquisition: Boolean): Either[String, GnirsFilter] = 
    SpectroscopyFilterTable
      .toSortedMap
      .collectFirst:
        case (filter, range) if range.contains(wavelength)  && (!isAcquisition || AcquisitionFilters.contains_(filter)) => filter
      .toRight(s"No Gnirs spectroscopy filter available for wavelength: $wavelength")



// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for GNIRS Filter.
 * @group Enumerations (Generated)
 */
enum GnirsFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Option[Wavelength],
  val spectroscopyRange: Option[BoundedInterval[Wavelength]], // Range of the spectroscopy filters.
) derives Enumerated, Display:
  case CrossDispersed extends GnirsFilter("CrossDispersed", "XD", "Cross dispersed", none, none)
  case Order6 extends GnirsFilter("Order6", "X", "Order 6 (X)", 1_100_000.pm.some, (1_103_000, 1_175_400).pmRange.some)
  case Order5 extends GnirsFilter("Order5", "J", "Order 5 (J)", 1_250_000.pm.some, (1_175_400, 1_370_000).pmRange.some)
  case Order4 extends GnirsFilter("Order4", "H", "Order 4 (H: 1.65µm)", 1_650_000.pm.some, (1_490_000, 1_800_000).pmRange.some)
  case Order3 extends GnirsFilter("Order3", "K", "Order 3 (K)", 2_200_000.pm.some, (1_910_000, 2_490_000).pmRange.some)
  case Order2 extends GnirsFilter("Order2", "L", "Order 2 (L)", 3_500_000.pm.some, (2_800_000, 4_200_000).pmRange.some)
  case Order1 extends GnirsFilter("Order1", "M", "Order 1 (M)", 4_800_000.pm.some, (4_400_000, 6_000_000).pmRange.some)
  case H2 extends GnirsFilter("H2", "H2", "H2: 2.12µm", 2_120_000.pm.some, none)
  case HNd100x extends GnirsFilter("HNd100x", "H+ND100X", "H + ND100X", 1_650_000.pm.some, none)
  case H2Nd100x extends GnirsFilter("H2Nd100x", "H2+ND100X", "H2 + ND100X", 2_120_000.pm.some, none)
  case PAH extends GnirsFilter("PAH", "PAH", "PAH: 3.3µm", 3_300_000.pm.some, none)
  case Y extends GnirsFilter("Y", "Y", "Y: 1.03µm", 1_030_000.pm.some, none)
  case J extends GnirsFilter("J", "J", "J: 1.25µm", 1_250_000.pm.some, none)
  case K extends GnirsFilter("K", "K", "K: 2.20µm", 2_200_000.pm.some, none)

object GnirsFilter:
  private val SpectroscopyFilterTable: List[(GnirsFilter, BoundedInterval[Wavelength])] = 
    values.map(f => f.spectroscopyRange.map(w => (f, w))).toList.flattenOption

  def fromSpectroscopyWavelength(wavelength: Wavelength): Either[String, GnirsFilter] = 
    SpectroscopyFilterTable
      .collectFirst:
        case (filter, range) if range.contains(wavelength) => filter
      .toRight(s"No Gnirs spectroscopy filter available for wavelength: $wavelength")

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.all.*
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
  val waveLength: Option[Wavelength],
  val spectroscopyCutoffWavelength: Option[Wavelength]
) derives Enumerated, Display:
  case CrossDispersed extends GnirsFilter("CrossDispersed", "XD", "Cross dispersed", none, none)
  case Order6 extends GnirsFilter("Order6", "X", "Order 6 (X)", 1_100_000.pm.some, 1_170_000.pm.some)
  case Order5 extends GnirsFilter("Order5", "J", "Order 5 (J)", 1_250_000.pm.some, 1_420_000.pm.some)
  case Order4 extends GnirsFilter("Order4", "H", "Order 4 (H: 1.65µm)", 1_650_000.pm.some, 1_860_000.pm.some)
  case Order3 extends GnirsFilter("Order3", "K", "Order 3 (K)", 2_200_000.pm.some, 2_700_000.pm.some)
  case Order2 extends GnirsFilter("Order2", "L", "Order 2 (L)", 3_500_000.pm.some, 4_300_000.pm.some)
  case Order1 extends GnirsFilter("Order1", "M", "Order 1 (M)", 4_800_000.pm.some, 6_000_000.pm.some)
  case H2 extends GnirsFilter("H2", "H2", "H2: 2.12µm", 2_120_000.pm.some, none)
  case HNd100x extends GnirsFilter("HNd100x", "H+ND100X", "H + ND100X", 1_650_000.pm.some, none)
  case H2Nd100x extends GnirsFilter("H2Nd100x", "H2+ND100X", "H2 + ND100X", 2_120_000.pm.some, none)
  case PAH extends GnirsFilter("PAH", "PAH", "PAH: 3.3µm", 3_300_000.pm.some, none)
  case Y extends GnirsFilter("Y", "Y", "Y: 1.03µm", 1_030_000.pm.some, none)
  case J extends GnirsFilter("J", "J", "J: 1.25µm", 1_250_000.pm.some, none)
  case K extends GnirsFilter("K", "K", "K: 2.20µm", 2_200_000.pm.some, none)

object GnirsFilter:
  private val SpectroscopyFilterTable: List[(GnirsFilter, Wavelength)] = 
    values.map(f => f.spectroscopyCutoffWavelength.map(w => (f, w))).toList.flattenOption

  // Adapted from seqexec
  def fromSpectroscopyWavelength(wavelength: Wavelength): GnirsFilter = 
    SpectroscopyFilterTable.foldRight[GnirsFilter](GnirsFilter.CrossDispersed):  (entry, selected) =>
      if (wavelength < entry._2) entry._1 else selected

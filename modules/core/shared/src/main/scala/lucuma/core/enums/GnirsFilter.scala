// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.math.Wavelength
import lucuma.core.util.Display
import lucuma.core.util.Enumerated


/**
 * Enumerated type for GNIRS Filter.
 * @group Enumerations (Generated)
 */
enum GnirsFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val waveLength: Option[Wavelength]
) derives Enumerated, Display:
  case CrossDispersed extends GnirsFilter("CrossDispersed", "XD", "Cross dispersed", Option.empty[Wavelength])
  case Order6 extends GnirsFilter("Order6", "X", "Order 6 (X)", Some(Wavelength.unsafeFromIntPicometers(1100000)))
  case Order5 extends GnirsFilter("Order5", "J", "Order 5 (J)", Some(Wavelength.unsafeFromIntPicometers(1250000)))
  case Order4 extends GnirsFilter("Order4", "H", "Order 4 (H: 1.65µm)", Some(Wavelength.unsafeFromIntPicometers(1650000)))
  case Order3 extends GnirsFilter("Order3", "K", "Order 3 (K)", Some(Wavelength.unsafeFromIntPicometers(2200000)))
  case Order2 extends GnirsFilter("Order2", "L", "Order 2 (L)", Some(Wavelength.unsafeFromIntPicometers(3500000)))
  case Order1 extends GnirsFilter("Order1", "M", "Order 1 (M)", Some(Wavelength.unsafeFromIntPicometers(4800000)))
  case H2 extends GnirsFilter("H2", "H2", "H2: 2.12µm", Some(Wavelength.unsafeFromIntPicometers(2120000)))
  case HNd100x extends GnirsFilter("HNd100x", "H+ND100X", "H + ND100X", Some(Wavelength.unsafeFromIntPicometers(1650000)))
  case H2Nd100x extends GnirsFilter("H2Nd100x", "H2+ND100X", "H2 + ND100X", Some(Wavelength.unsafeFromIntPicometers(2120000)))
  case PAH extends GnirsFilter("PAH", "PAH", "PAH: 3.3µm", Some(Wavelength.unsafeFromIntPicometers(3300000)))
  case Y extends GnirsFilter("Y", "Y", "Y: 1.03µm", Some(Wavelength.unsafeFromIntPicometers(1030000)))
  case J extends GnirsFilter("J", "J", "J: 1.25µm", Some(Wavelength.unsafeFromIntPicometers(1250000)))
  case K extends GnirsFilter("K", "K", "K: 2.20µm", Some(Wavelength.unsafeFromIntPicometers(2200000)))
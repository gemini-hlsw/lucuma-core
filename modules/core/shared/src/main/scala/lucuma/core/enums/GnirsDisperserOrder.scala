// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.util.Display
import lucuma.core.util.Enumerated


/**
 * Enumerated type for GNIRS Disperser Order.
 *
 * `deltaWavelength` is the wavelength covered by one pixel with the 32 l/mm disperser at
 * the 0.15"/pix pixel scale; the other combinations are derived from it (111 l/mm divides
 * by 3.49, 10 l/mm multiplies by 3, and the 0.05"/pix scale divides by 3).  It is `None`
 * for the two orders where it is undefined, which are also the two that cannot be
 * cross-dispersed.
 *
 * Note that order 4 appears twice: `FourXD` is the cross-dispersed variant, and both have
 * a `count` of 4.
 *
 * @group Enumerations
 */
enum GnirsDisperserOrder(
  val tag:               String,
  val shortName:         String,
  val longName:          String,
  val count:             Int,
  val defaultWavelength: Wavelength,
  val minWavelength:     Wavelength,
  val maxWavelength:     Wavelength,
  val deltaWavelength:   Option[WavelengthDelta],
  val band:              Option[Band],
  val crossDispersed:    Boolean
) derives Enumerated, Display:
  case One extends GnirsDisperserOrder("One", "1", "One", 1, Wavelength.unsafeFromIntPicometers(4850000), Wavelength.unsafeFromIntPicometers(4300000), Wavelength.unsafeFromIntPicometers(6000000), None, Some(Band.M), false)
  case Two extends GnirsDisperserOrder("Two", "2", "Two", 2, Wavelength.unsafeFromIntPicometers(3400000), Wavelength.unsafeFromIntPicometers(2700000), Wavelength.unsafeFromIntPicometers(4300000), None, Some(Band.L), false)
  case Three extends GnirsDisperserOrder("Three", "3", "Three", 3, Wavelength.unsafeFromIntPicometers(2220000), Wavelength.unsafeFromIntPicometers(1860000), Wavelength.unsafeFromIntPicometers(2700000), Some(WavelengthDelta.unsafeFromIntPicometers(647)), Some(Band.K), true)
  case FourXD extends GnirsDisperserOrder("FourXD", "4XD", "FourXD", 4, Wavelength.unsafeFromIntPicometers(1650000), Wavelength.unsafeFromIntPicometers(1420000), Wavelength.unsafeFromIntPicometers(1860000), Some(WavelengthDelta.unsafeFromIntPicometers(482)), Some(Band.H), true)
  case Four extends GnirsDisperserOrder("Four", "4", "Four", 4, Wavelength.unsafeFromIntPicometers(1630000), Wavelength.unsafeFromIntPicometers(1420000), Wavelength.unsafeFromIntPicometers(1860000), Some(WavelengthDelta.unsafeFromIntPicometers(485)), Some(Band.H), true)
  case Five extends GnirsDisperserOrder("Five", "5", "Five", 5, Wavelength.unsafeFromIntPicometers(1250000), Wavelength.unsafeFromIntPicometers(1170000), Wavelength.unsafeFromIntPicometers(1420000), Some(WavelengthDelta.unsafeFromIntPicometers(388)), Some(Band.J), true)
  case Six extends GnirsDisperserOrder("Six", "6", "Six", 6, Wavelength.unsafeFromIntPicometers(1100000), Wavelength.unsafeFromIntPicometers(1030000), Wavelength.unsafeFromIntPicometers(1170000), Some(WavelengthDelta.unsafeFromIntPicometers(323)), None, true)
  case Seven extends GnirsDisperserOrder("Seven", "7", "Seven", 7, Wavelength.unsafeFromIntPicometers(951000), Wavelength.unsafeFromIntPicometers(880000), Wavelength.unsafeFromIntPicometers(1030000), Some(WavelengthDelta.unsafeFromIntPicometers(276)), None, true)
  case Eight extends GnirsDisperserOrder("Eight", "8", "Eight", 8, Wavelength.unsafeFromIntPicometers(832000), Wavelength.unsafeFromIntPicometers(780000), Wavelength.unsafeFromIntPicometers(880000), Some(WavelengthDelta.unsafeFromIntPicometers(241)), None, true)

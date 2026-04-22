// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import lucuma.core.math.Wavelength
import lucuma.core.util.Display
import lucuma.core.util.Enumerated


/**
 * Enumerated type for GNIRS Disperser Order.
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
  val deltaWavelength:   Wavelength,
  val band:              Option[Band],
  val crossDispersed:    Boolean
) derives Enumerated, Display:
  case One extends GnirsDisperserOrder("One", "1", "One", 1, Wavelength.unsafeFromIntPicometers(4850000), Wavelength.unsafeFromIntPicometers(4300000), Wavelength.unsafeFromIntPicometers(6000000), Wavelength.unsafeFromIntPicometers(0), Some(Band.M), false)
  case Two extends GnirsDisperserOrder("Two", "2", "Two", 2, Wavelength.unsafeFromIntPicometers(3400000), Wavelength.unsafeFromIntPicometers(2700000), Wavelength.unsafeFromIntPicometers(4300000), Wavelength.unsafeFromIntPicometers(0), Some(Band.L), false)
  case Three extends GnirsDisperserOrder("Three", "3", "Three", 3, Wavelength.unsafeFromIntPicometers(2220000), Wavelength.unsafeFromIntPicometers(1860000), Wavelength.unsafeFromIntPicometers(2700000), Wavelength.unsafeFromIntPicometers(647), Some(Band.K), true)
  case FourXD extends GnirsDisperserOrder("FourXD", "4XD", "FourXD", 4, Wavelength.unsafeFromIntPicometers(1650000), Wavelength.unsafeFromIntPicometers(1420000), Wavelength.unsafeFromIntPicometers(1860000), Wavelength.unsafeFromIntPicometers(482), Some(Band.H), true)
  case Four extends GnirsDisperserOrder("Four", "4", "Four", 4, Wavelength.unsafeFromIntPicometers(1630000), Wavelength.unsafeFromIntPicometers(1420000), Wavelength.unsafeFromIntPicometers(1860000), Wavelength.unsafeFromIntPicometers(485), Some(Band.H), true)
  case Five extends GnirsDisperserOrder("Five", "5", "Five", 5, Wavelength.unsafeFromIntPicometers(1250000), Wavelength.unsafeFromIntPicometers(1170000), Wavelength.unsafeFromIntPicometers(1420000), Wavelength.unsafeFromIntPicometers(388), Some(Band.J), true)
  case Six extends GnirsDisperserOrder("Six", "6", "Six", 6, Wavelength.unsafeFromIntPicometers(1100000), Wavelength.unsafeFromIntPicometers(1030000), Wavelength.unsafeFromIntPicometers(1170000), Wavelength.unsafeFromIntPicometers(323), None, true)
  case Seven extends GnirsDisperserOrder("Seven", "7", "Seven", 7, Wavelength.unsafeFromIntPicometers(951000), Wavelength.unsafeFromIntPicometers(880000), Wavelength.unsafeFromIntPicometers(1030000), Wavelength.unsafeFromIntPicometers(276), None, true)
  case Eight extends GnirsDisperserOrder("Eight", "8", "Eight", 8, Wavelength.unsafeFromIntPicometers(832000), Wavelength.unsafeFromIntPicometers(780000), Wavelength.unsafeFromIntPicometers(880000), Wavelength.unsafeFromIntPicometers(241), None, true)
  
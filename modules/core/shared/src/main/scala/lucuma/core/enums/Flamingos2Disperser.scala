// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 dispersers.
 * @group Enumerations (Generated)
 */
enum Flamingos2Disperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength
) derives Enumerated:

  case R1200JH extends Flamingos2Disperser("R1200JH", "R1200JH", "R=1200 (J + H) grism",       Wavelength.unsafeFromIntPicometers(1390000))
  case R1200HK extends Flamingos2Disperser("R1200HK", "R1200HK", "R=1200 (H + K) grism",       Wavelength.unsafeFromIntPicometers(1871000))
  case R3000   extends Flamingos2Disperser("R3000",   "R3000",   "R=3000 (J or H or K) grism", Wavelength.unsafeFromIntPicometers(1650000))


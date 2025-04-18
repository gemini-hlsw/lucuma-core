// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 filters.
 * @group Enumerations (Generated)
 */
enum F2Filter(val tag: String, val shortName: String, val longName: String, val wavelength: Option[Wavelength]) derives Enumerated:
  case Y      extends F2Filter("Y",      "Y",       "Y (1.02 um)",        Wavelength.fromIntPicometers(1020000))
  case J      extends F2Filter("J",      "J",       "J (1.25 um)",        Wavelength.fromIntPicometers(1250000))
  case H      extends F2Filter("H",      "H",       "H (1.65 um)",        Wavelength.fromIntPicometers(1650000))
  case JH     extends F2Filter("JH",     "JH",      "JH (spectroscopic)", Wavelength.fromIntPicometers(1390000))
  case HK     extends F2Filter("HK",     "HK",      "HK (spectroscopic)", Wavelength.fromIntPicometers(1871000))
  case JLow   extends F2Filter("JLow",   "J-low",   "J-low (1.15 um)",    Wavelength.fromIntPicometers(1150000))
  case KLong  extends F2Filter("KLong",  "K-long",  "K-long (2.20 um)",   Wavelength.fromIntPicometers(2200000))
  case KShort extends F2Filter("KShort", "K-short", "K-short (2.15 um)",  Wavelength.fromIntPicometers(2150000))
  case KBlue  extends F2Filter("KBlue",  "K-blue",  "K-blue (2.06 um)",   Wavelength.fromIntPicometers(2060000))
  case KRed   extends F2Filter("KRed",   "K-red",   "K-red (2.31 um)",    Wavelength.fromIntPicometers(2310000))

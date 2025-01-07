// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 filters.
 * @group Enumerations (Generated)
 */
enum F2Filter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Option[Wavelength],
  val obsolete: Boolean
) derives Enumerated:

  case Y      extends F2Filter("Y",      "Y",       "Y (1.02 um)",        Wavelength.fromIntPicometers(1020000), false)
  case F1056  extends F2Filter("F1056",  "F1056",   "F1056 (1.056 um)",   Wavelength.fromIntPicometers(1056000), false)
  case J      extends F2Filter("J",      "J",       "J (1.25 um)",        Wavelength.fromIntPicometers(1250000), false)
  case H      extends F2Filter("H",      "H",       "H (1.65 um)",        Wavelength.fromIntPicometers(1650000), false)
  case JH     extends F2Filter("JH",     "JH",      "JH (spectroscopic)", Wavelength.fromIntPicometers(1390000), false)
  case HK     extends F2Filter("HK",     "HK",      "HK (spectroscopic)", Wavelength.fromIntPicometers(1871000), false)
  case JLow   extends F2Filter("JLow",   "J-low",   "J-low (1.15 um)",    Wavelength.fromIntPicometers(1150000), false)
  case KLong  extends F2Filter("KLong",  "K-long",  "K-long (2.20 um)",   Wavelength.fromIntPicometers(2200000), false)
  case KShort extends F2Filter("KShort", "K-short", "K-short (2.15 um)",  Wavelength.fromIntPicometers(2150000), false)
  case F1063  extends F2Filter("F1063",  "F1063",   "F1063 (1.063 um)",   Wavelength.fromIntPicometers(1063000), false)
  case KBlue  extends F2Filter("KBlue",  "K-blue",  "K-blue (2.06 um)",   Wavelength.fromIntPicometers(2060000), false)
  case KRed   extends F2Filter("KRed",   "K-red",   "K-red (2.31 um)",    Wavelength.fromIntPicometers(2310000), false)
  case Open   extends F2Filter("Open",   "Open",    "Open",               Wavelength.fromIntPicometers(1600000), true)
  case Dark   extends F2Filter("Dark",   "Dark",    "Dark",               Option.empty[Wavelength],                          true)

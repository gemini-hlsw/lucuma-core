// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for Flamingos2 filters.
 * @group Enumerations
 */
enum F2Filter(val tag: String, val shortName: String, val longName: String, val wavelength: Wavelength) derives Enumerated:
  case Y      extends F2Filter("Y",      "Y",       "Y (1.02 um)",        1020000.pm)
  case J      extends F2Filter("J",      "J",       "J (1.25 um)",        1250000.pm)
  case H      extends F2Filter("H",      "H",       "H (1.65 um)",        1650000.pm)
  case JH     extends F2Filter("JH",     "JH",      "JH (spectroscopic)", 1390000.pm)
  case HK     extends F2Filter("HK",     "HK",      "HK (spectroscopic)", 1871000.pm)
  case JLow   extends F2Filter("JLow",   "J-low",   "J-low (1.15 um)",    1150000.pm)
  case KLong  extends F2Filter("KLong",  "K-long",  "K-long (2.20 um)",   2200000.pm)
  case KShort extends F2Filter("KShort", "K-short", "K-short (2.15 um)",  2150000.pm)
  case KBlue  extends F2Filter("KBlue",  "K-blue",  "K-blue (2.06 um)",   2060000.pm)
  case KRed   extends F2Filter("KRed",   "K-red",   "K-red (2.31 um)",    2310000.pm)

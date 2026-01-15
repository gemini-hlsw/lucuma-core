// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for Flamingos2 filters.
 * @group Enumerations
 */
enum Flamingos2Filter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val supportsSpectroscopy: Boolean
) derives Enumerated:
  case Y      extends Flamingos2Filter("Y",      "Y",       "Y (1.02 um)",        1020000.pm, true)
  case J      extends Flamingos2Filter("J",      "J",       "J (1.25 um)",        1250000.pm, true)
  case H      extends Flamingos2Filter("H",      "H",       "H (1.65 um)",        1650000.pm, true)
  case JH     extends Flamingos2Filter("JH",     "JH",      "JH (spectroscopic)", 1390000.pm, true)
  case HK     extends Flamingos2Filter("HK",     "HK",      "HK (spectroscopic)", 1871000.pm, true)
  case JLow   extends Flamingos2Filter("JLow",   "J-low",   "J-low (1.15 um)",    1150000.pm, true)
  case KLong  extends Flamingos2Filter("KLong",  "K-long",  "K-long (2.20 um)",   2200000.pm, true)
  case KShort extends Flamingos2Filter("KShort", "K-short", "K-short (2.15 um)",  2150000.pm, true)
  case KBlue  extends Flamingos2Filter("KBlue",  "K-blue",  "K-blue (2.06 um)",   2060000.pm, false)
  case KRed   extends Flamingos2Filter("KRed",   "K-red",   "K-red (2.31 um)",    2310000.pm, false)

object Flamingos2Filter:

  /** Acquisition filter options. */
  val acquisition: NonEmptyList[Flamingos2Filter] =
    NonEmptyList.of(J, H, KShort)

  /** Filters usable in spectrosocpy */
  val spectroscopic: NonEmptyList[Flamingos2Filter] =
    // KBlue and KRed cannot be used in spectroscopy
    NonEmptyList.fromListUnsafe(Enumerated[Flamingos2Filter].all.filter(_.supportsSpectroscopy))

  val spectroscopyFilters: Enumerated[Flamingos2Filter] =
    Enumerated.fromNEL(spectroscopic).withTag(_.tag)

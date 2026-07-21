// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for Flamingos2 filters.
 *
 * The `width` values are derived from the "Band Width [nm]" column of the Gemini Flamingos-2
 * filter table (https://www.gemini.edu/instrumentation/flamingos-2/components#Filters), expressed
 * as an interval centered on each filter's wavelength. There is some uncertainty in these values
 * as the source page does not document the precise bandpass definition used for that column; the
 * value used for each filter is noted in a comment above its case.
 * @group Enumerations
 */
enum Flamingos2Filter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val width: BoundedInterval[Wavelength],
  val supportsSpectroscopy: Boolean,
  val filterType: FilterType
) derives Enumerated:
  // Band width: 89.4 nm
  case Y      extends Flamingos2Filter("Y",      "Y",       "Y (1.02 um)",        1_020_000.pm, (975_300,   1_064_700).pmRange, true,  FilterType.BroadBand)
  // Band width: 151 nm
  case J      extends Flamingos2Filter("J",      "J",       "J (1.25 um)",        1_250_000.pm, (1_174_500, 1_325_500).pmRange, true,  FilterType.BroadBand)
  // Band width: 274 nm
  case H      extends Flamingos2Filter("H",      "H",       "H (1.65 um)",        1_650_000.pm, (1_513_000, 1_787_000).pmRange, true,  FilterType.BroadBand)
  // Band width: 900 nm
  case JH     extends Flamingos2Filter("JH",     "JH",      "JH (spectroscopic)", 1_390_000.pm, (940_000,   1_840_000).pmRange, true,  FilterType.BroadBand)
  // Band width: 1162.5 nm
  case HK     extends Flamingos2Filter("HK",     "HK",      "HK (spectroscopic)", 1_871_000.pm, (1_289_750, 2_452_250).pmRange, true,  FilterType.BroadBand)
  // Band width: 132 nm
  case JLow   extends Flamingos2Filter("JLow",   "J-low",   "J-low (1.15 um)",    1_150_000.pm, (1_084_000, 1_216_000).pmRange, true,  FilterType.BroadBand)
  // Band width: 600 nm
  case KLong  extends Flamingos2Filter("KLong",  "K-long",  "K-long (2.20 um)",   2_200_000.pm, (1_900_000, 2_500_000).pmRange, true,  FilterType.BroadBand)
  // Band width: 318 nm
  case KShort extends Flamingos2Filter("KShort", "K-short", "K-short (2.15 um)",  2_150_000.pm, (1_991_000, 2_309_000).pmRange, true,  FilterType.BroadBand)
  // Band width: 227 nm
  case KBlue  extends Flamingos2Filter("KBlue",  "K-blue",  "K-blue (2.06 um)",   2_060_000.pm, (1_946_500, 2_173_500).pmRange, false, FilterType.BroadBand)
  // Band width: 248 nm
  case KRed   extends Flamingos2Filter("KRed",   "K-red",   "K-red (2.31 um)",    2_310_000.pm, (2_186_000, 2_434_000).pmRange, false, FilterType.BroadBand)

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

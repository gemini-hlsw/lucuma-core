// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.data.NonEmptyList
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

import ConvenienceOps.*

/**
 * Enumerated type for GMOS South filters.
 */
enum GmosSouthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val width: BoundedInterval[Wavelength],
  val filterType: FilterType
) derives Enumerated:

  case UPrime           extends GmosSouthFilter("UPrime",           "u",       "u_G0332",                    350_000.pm, (336_000,   385_000).pmRange, FilterType.BroadBand)
  case GPrime           extends GmosSouthFilter("GPrime",           "g",       "g_G0325",                    475_000.pm, (398_000,   552_000).pmRange, FilterType.BroadBand)
  case RPrime           extends GmosSouthFilter("RPrime",           "r",       "r_G0326",                    630_000.pm, (562_000,   698_000).pmRange, FilterType.BroadBand)
  case IPrime           extends GmosSouthFilter("IPrime",           "i",       "i_G0327",                    780_000.pm, (706_000,   850_000).pmRange, FilterType.BroadBand)
  case ZPrime           extends GmosSouthFilter("ZPrime",           "z",       "z_G0328",                    925_000.pm, 848_000.gePmRange,            FilterType.BroadBand)
  case Z                extends GmosSouthFilter("Z",                "Z",       "Z_G0343",                    876_000.pm, (830_000,   925_000).pmRange, FilterType.BroadBand)
  case Y                extends GmosSouthFilter("Y",                "Y",       "Y_G0344",                  1_010_000.pm, (970_000, 1_070_000).pmRange, FilterType.BroadBand)
  case GG455            extends GmosSouthFilter("GG455",            "GG455",   "GG455_G0329",                555_000.pm, 460_000.gePmRange,            FilterType.Spectroscopic)
  case OG515            extends GmosSouthFilter("OG515",            "OG515",   "OG515_G0330",                615_000.pm, 520_000.gePmRange,            FilterType.Spectroscopic)
  case RG610            extends GmosSouthFilter("RG610",            "RG610",   "RG610_G0331",                710_000.pm, 615_000.gePmRange,            FilterType.Spectroscopic)
  case RG780            extends GmosSouthFilter("RG780",            "RG780",   "RG780_G0334",                880_000.pm, 780_000.gePmRange,            FilterType.Spectroscopic)
  case CaT              extends GmosSouthFilter("CaT",              "CaT",     "CaT_G0333",                  860_000.pm, (780_000,   933_000).pmRange, FilterType.BroadBand)
  case HartmannA_RPrime extends GmosSouthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0337 + r_G0326",  630_000.pm, 630_000.gePmRange,            FilterType.Engineering)
  case HartmannB_RPrime extends GmosSouthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0338 + r_G0326",  630_000.pm, 630_000.gePmRange,            FilterType.Engineering)
  case GPrime_GG455     extends GmosSouthFilter("GPrime_GG455",     "g+GG455", "g_G0325 + GG455_G0329",      506_000.pm, (460_000,   552_000).pmRange, FilterType.Combination)
  case GPrime_OG515     extends GmosSouthFilter("GPrime_OG515",     "g+OG515", "g_G0325 + OG515_G0330",      536_000.pm, (520_000,   552_000).pmRange, FilterType.Combination)
  case RPrime_RG610     extends GmosSouthFilter("RPrime_RG610",     "r+RG610", "r_G0326 + RG610_G0331",      657_000.pm, (615_000,   698_000).pmRange, FilterType.Combination)
  case IPrime_RG780     extends GmosSouthFilter("IPrime_RG780",     "i+RG780", "i_G0327 + RG780_G0334",      819_000.pm, (777_000,   851_000).pmRange, FilterType.Combination)
  case IPrime_CaT       extends GmosSouthFilter("IPrime_CaT",       "i+CaT",   "i_G0327 + CaT_G0333",        815_000.pm, (780_000,   850_000).pmRange, FilterType.Combination)
  case ZPrime_CaT       extends GmosSouthFilter("ZPrime_CaT",       "z+CaT",   "z_G0328 + CaT_G0333",        890_000.pm, (848_000,   933_000).pmRange, FilterType.Combination)
  case Ha               extends GmosSouthFilter("Ha",               "Ha",      "Ha_G0336",                   656_000.pm, (654_000,   661_000).pmRange, FilterType.NarrowBand)
  case SII              extends GmosSouthFilter("SII",              "SII",     "SII_G0335",                  672_000.pm, (669_400,   673_700).pmRange, FilterType.NarrowBand)
  case HaC              extends GmosSouthFilter("HaC",              "HaC",     "HaC_G0337",                  662_000.pm, (659_000,   665_000).pmRange, FilterType.NarrowBand)
  case OIII             extends GmosSouthFilter("OIII",             "OIII",    "OIII_G0338",                 499_000.pm, (496_500,   501_500).pmRange, FilterType.NarrowBand)
  case OIIIC            extends GmosSouthFilter("OIIIC",            "OIIIC",   "OIIIC_G0339",                514_000.pm, (509_000,   519_000).pmRange, FilterType.NarrowBand)
  case HeII             extends GmosSouthFilter("HeII",             "HeII",    "HeII_G0340",                 468_000.pm, (464_000,   472_000).pmRange, FilterType.NarrowBand)
  case HeIIC            extends GmosSouthFilter("HeIIC",            "HeIIC",   "HeIIC_G0341",                478_000.pm, (474_000,   482_000).pmRange, FilterType.NarrowBand)
  case OVI              extends GmosSouthFilter("OVI",              "OVI",     "OVI_G0347",                  684_000.pm, (681_600,   686_500).pmRange, FilterType.NarrowBand)
  case OVIC             extends GmosSouthFilter("OVIC",             "OVIC",    "OVI_G0348",                  679_000.pm, (676_100,   680_900).pmRange, FilterType.NarrowBand)

object GmosSouthFilter:
  /** Acquisition filter options. */
  val acquisition: NonEmptyList[GmosSouthFilter] =
    NonEmptyList.fromListUnsafe(
      List(UPrime, GPrime, RPrime, IPrime, ZPrime)
    )

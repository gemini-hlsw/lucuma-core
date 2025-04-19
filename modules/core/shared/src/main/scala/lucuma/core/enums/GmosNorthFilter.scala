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
 * Enumerated type for GMOS North filters.
 * @group Enumerations (Generated)
 */
enum GmosNorthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val width: BoundedInterval[Wavelength],
  val filterType: FilterType) derives Enumerated:

  case GPrime           extends GmosNorthFilter("GPrime",           "g",       "g_G0301",                   475_000.pm, (398_000,   552_000).pmRange, FilterType.BroadBand)
  case RPrime           extends GmosNorthFilter("RPrime",           "r",       "r_G0303",                   630_000.pm, (562_000,   698_000).pmRange, FilterType.BroadBand)
  case IPrime           extends GmosNorthFilter("IPrime",           "i",       "i_G0302",                   780_000.pm, (706_000,   850_000).pmRange, FilterType.BroadBand)
  case ZPrime           extends GmosNorthFilter("ZPrime",           "z",       "z_G0304",                   925_000.pm, 848_000.gePmRange,            FilterType.BroadBand)
  case Z                extends GmosNorthFilter("Z",                "Z",       "Z_G0322",                   876_000.pm, (830_000,   925_000).pmRange, FilterType.BroadBand)
  case Y                extends GmosNorthFilter("Y",                "Y",       "Y_G0323",                 1_010_000.pm, (970_000, 1_070_000).pmRange, FilterType.BroadBand)
  case Ri               extends GmosNorthFilter("Ri",               "r+i",     "ri_G0349",                  705_000.pm, (560_000,   850_000).pmRange, FilterType.BroadBand)
  case GG455            extends GmosNorthFilter("GG455",            "GG455",   "GG455_G0305",               555_000.pm, 460_000.gePmRange,            FilterType.Spectroscopic)
  case OG515            extends GmosNorthFilter("OG515",            "OG515",   "OG515_G0306",               615_000.pm, 520_000.gePmRange,            FilterType.Spectroscopic)
  case RG610            extends GmosNorthFilter("RG610",            "RG610",   "RG610_G0307",               710_000.pm, 615_000.gePmRange,            FilterType.Spectroscopic)
  case CaT              extends GmosNorthFilter("CaT",              "CaT",     "CaT_G0309",                 860_000.pm, (780_000,   933_000).pmRange, FilterType.BroadBand)
  case Ha               extends GmosNorthFilter("Ha",               "Ha",      "Ha_G0310",                  656_000.pm, (654_000,   661_000).pmRange, FilterType.NarrowBand)
  case HaC              extends GmosNorthFilter("HaC",              "HaC",     "HaC_G0311",                 662_000.pm, (659_000,   665_000).pmRange, FilterType.NarrowBand)
  case DS920            extends GmosNorthFilter("DS920",            "DS920",   "DS920_G0312",               920_000.pm, (912_800,   931_400).pmRange, FilterType.NarrowBand)
  case SII              extends GmosNorthFilter("SII",              "SII",     "SII_G0317",                 672_000.pm, (669_400,   673_700).pmRange, FilterType.NarrowBand)
  case OIII             extends GmosNorthFilter("OIII",             "OIII",    "OIII_G0318",                499_000.pm, (496_500,   501_500).pmRange, FilterType.NarrowBand)
  case OIIIC            extends GmosNorthFilter("OIIIC",            "OIIIC",   "OIIIC_G0319",               514_000.pm, (509_000,   519_000).pmRange, FilterType.NarrowBand)
  case HeII             extends GmosNorthFilter("HeII",             "HeII",    "HeII_G0320",                468_000.pm, (464_000,   472_000).pmRange, FilterType.NarrowBand)
  case HeIIC            extends GmosNorthFilter("HeIIC",            "HeIIC",   "HeIIC_G0321",               478_000.pm, (474_000,   482_000).pmRange, FilterType.NarrowBand)
  case OVI              extends GmosNorthFilter("OVI",              "OVI",     "OVI_G0345",                 684_000.pm, (681_600,   686_500).pmRange, FilterType.NarrowBand)
  case OVIC             extends GmosNorthFilter("OVIC",             "OVIC",    "OVIC_G0346",                679_000.pm, (676_100,   680_900).pmRange, FilterType.NarrowBand)
  case HartmannA_RPrime extends GmosNorthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0313 + r_G0303", 630_000.pm, 630_000.gePmRange,            FilterType.Engineering)
  case HartmannB_RPrime extends GmosNorthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0314 + r_G0303", 630_000.pm, 630_000.gePmRange,            FilterType.Engineering)
  case GPrime_GG455     extends GmosNorthFilter("GPrime_GG455",     "g+GG455", "g_G0301 + GG455_G0305",     506_000.pm, (460_000,   552_000).pmRange, FilterType.Combination)
  case GPrime_OG515     extends GmosNorthFilter("GPrime_OG515",     "g+OG515", "g_G0301 + OG515_G0306",     536_000.pm, (520_000,   552_000).pmRange, FilterType.Combination)
  case RPrime_RG610     extends GmosNorthFilter("RPrime_RG610",     "r+RG610", "r_G0303 + RG610_G0307",     657_000.pm, (615_000,   698_000).pmRange, FilterType.Combination)
  case IPrime_CaT       extends GmosNorthFilter("IPrime_CaT",       "i+CaT",   "i_G0302 + CaT_G0309",       815_000.pm, (780_000,   850_000).pmRange, FilterType.Combination)
  case ZPrime_CaT       extends GmosNorthFilter("ZPrime_CaT",       "z+CaT",   "z_G0305 + CaT_G0309",       890_000.pm, (848_000,   933_000).pmRange, FilterType.Combination)


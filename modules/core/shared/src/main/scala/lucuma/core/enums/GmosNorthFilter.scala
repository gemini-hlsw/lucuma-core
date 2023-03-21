// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.data.NonEmptyList
import cats.syntax.all._
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS North filters.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosNorthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val obsolete: Boolean,
  val width: BoundedInterval[Wavelength],
  val filterType: FilterType
) extends Product with Serializable

object GmosNorthFilter {

  import ConvenienceOps.*

  /** @group Constructors */ case object GPrime           extends GmosNorthFilter("GPrime",           "g",       "g_G0301",                   475_000.pm, false, (398_000,   552_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object RPrime           extends GmosNorthFilter("RPrime",           "r",       "r_G0303",                   630_000.pm, false, (562_000,   698_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object IPrime           extends GmosNorthFilter("IPrime",           "i",       "i_G0302",                   780_000.pm, false, (706_000,   850_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object ZPrime           extends GmosNorthFilter("ZPrime",           "z",       "z_G0304",                   925_000.pm, false,  848_000.gePmRange,           FilterType.BroadBand)
  /** @group Constructors */ case object Z                extends GmosNorthFilter("Z",                "Z",       "Z_G0322",                   876_000.pm, false, (830_000,   925_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object Y                extends GmosNorthFilter("Y",                "Y",       "Y_G0323",                 1_010_000.pm, false, (970_000, 1_070_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object Ri               extends GmosNorthFilter("Ri",               "r+i",     "ri_G0349",                  705_000.pm, false, (560_000,   850_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object GG455            extends GmosNorthFilter("GG455",            "GG455",   "GG455_G0305",               555_000.pm, false,  460_000.gePmRange,           FilterType.Spectroscopic)
  /** @group Constructors */ case object OG515            extends GmosNorthFilter("OG515",            "OG515",   "OG515_G0306",               615_000.pm, false,  520_000.gePmRange,           FilterType.Spectroscopic)
  /** @group Constructors */ case object RG610            extends GmosNorthFilter("RG610",            "RG610",   "RG610_G0307",               710_000.pm, false,  615_000.gePmRange,           FilterType.Spectroscopic)
  /** @group Constructors */ case object CaT              extends GmosNorthFilter("CaT",              "CaT",     "CaT_G0309",                 860_000.pm, false, (780_000,   933_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object Ha               extends GmosNorthFilter("Ha",               "Ha",      "Ha_G0310",                  656_000.pm, false, (654_000,   661_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HaC              extends GmosNorthFilter("HaC",              "HaC",     "HaC_G0311",                 662_000.pm, false, (659_000,   665_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object DS920            extends GmosNorthFilter("DS920",            "DS920",   "DS920_G0312",               920_000.pm, false, (912_800,   931_400).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object SII              extends GmosNorthFilter("SII",              "SII",     "SII_G0317",                 672_000.pm, false, (669_400,   673_700).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OIII             extends GmosNorthFilter("OIII",             "OIII",    "OIII_G0318",                499_000.pm, false, (496_500,   501_500).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OIIIC            extends GmosNorthFilter("OIIIC",            "OIIIC",   "OIIIC_G0319",               514_000.pm, false, (509_000,   519_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HeII             extends GmosNorthFilter("HeII",             "HeII",    "HeII_G0320",                468_000.pm, false, (464_000,   472_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HeIIC            extends GmosNorthFilter("HeIIC",            "HeIIC",   "HeIIC_G0321",               478_000.pm, false, (474_000,   482_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OVI              extends GmosNorthFilter("OVI",              "OVI",     "OVI_G0345",                 684_000.pm, false, (681_600,   686_500).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OVIC             extends GmosNorthFilter("OVIC",             "OVIC",    "OVIC_G0346",                679_000.pm, false, (676_100,   680_900).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosNorthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0313 + r_G0303", 630_000.pm, false,  630_000.gePmRange,           FilterType.Engineering)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosNorthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0314 + r_G0303", 630_000.pm, false,  630_000.gePmRange,           FilterType.Engineering)
  /** @group Constructors */ case object GPrime_GG455     extends GmosNorthFilter("GPrime_GG455",     "g+GG455", "g_G0301 + GG455_G0305",     506_000.pm, false, (460_000,   552_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object GPrime_OG515     extends GmosNorthFilter("GPrime_OG515",     "g+OG515", "g_G0301 + OG515_G0306",     536_000.pm, false, (520_000,   552_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object RPrime_RG610     extends GmosNorthFilter("RPrime_RG610",     "r+RG610", "r_G0303 + RG610_G0307",     657_000.pm, false, (615_000,   698_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object IPrime_CaT       extends GmosNorthFilter("IPrime_CaT",       "i+CaT",   "i_G0302 + CaT_G0309",       815_000.pm, false, (780_000,   850_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object ZPrime_CaT       extends GmosNorthFilter("ZPrime_CaT",       "z+CaT",   "z_G0305 + CaT_G0309",       890_000.pm, false, (848_000,   933_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object UPrime           extends GmosNorthFilter("UPrime",           "u",       "u_G0308",                   350_000.pm, true,  (336_000,   385_000).pmRange, FilterType.BroadBand)

  /** All members of GmosNorthFilter, in canonical order. */
  val all: List[GmosNorthFilter] =
    List(GPrime, RPrime, IPrime, ZPrime, Z, Y, Ri, GG455, OG515, RG610, CaT, Ha, HaC, DS920, SII, OIII, OIIIC, HeII, HeIIC, OVI, OVIC, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_CaT, ZPrime_CaT, UPrime)

  /** Acquisition filter options. */
  val acquisition: NonEmptyList[GmosNorthFilter] =
    NonEmptyList.of(GPrime, RPrime, IPrime, ZPrime, UPrime)

  /** Select the member of GmosNorthFilter with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthFilter] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthFilter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosNorthFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosNorthFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosNorthFilterEnumerated: Enumerated[GmosNorthFilter] =
    new Enumerated[GmosNorthFilter] {
      def all = GmosNorthFilter.all
      def tag(a: GmosNorthFilter) = a.tag
      override def unsafeFromTag(s: String): GmosNorthFilter =
        GmosNorthFilter.unsafeFromTag(s)
    }

}

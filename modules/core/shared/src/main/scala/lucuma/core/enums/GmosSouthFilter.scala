// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS South filters.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val width: BoundedInterval[Wavelength],
  val filterType: FilterType
) extends Product with Serializable

object GmosSouthFilter {

  import ConvenienceOps.*

  /** @group Constructors */ case object UPrime           extends GmosSouthFilter("UPrime",           "u",       "u_G0332",                    350_000.pm, (336_000,   385_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object GPrime           extends GmosSouthFilter("GPrime",           "g",       "g_G0325",                    475_000.pm, (398_000,   552_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object RPrime           extends GmosSouthFilter("RPrime",           "r",       "r_G0326",                    630_000.pm, (562_000,   698_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object IPrime           extends GmosSouthFilter("IPrime",           "i",       "i_G0327",                    780_000.pm, (706_000,   850_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object ZPrime           extends GmosSouthFilter("ZPrime",           "z",       "z_G0328",                    925_000.pm, 848_000.gePmRange,            FilterType.BroadBand)
  /** @group Constructors */ case object Z                extends GmosSouthFilter("Z",                "Z",       "Z_G0343",                    876_000.pm, (830_000,   925_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object Y                extends GmosSouthFilter("Y",                "Y",       "Y_G0344",                  1_010_000.pm, (970_000, 1_070_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object GG455            extends GmosSouthFilter("GG455",            "GG455",   "GG455_G0329",                555_000.pm, 460_000.gePmRange,            FilterType.Spectroscopic)
  /** @group Constructors */ case object OG515            extends GmosSouthFilter("OG515",            "OG515",   "OG515_G0330",                615_000.pm, 520_000.gePmRange,            FilterType.Spectroscopic)
  /** @group Constructors */ case object RG610            extends GmosSouthFilter("RG610",            "RG610",   "RG610_G0331",                710_000.pm, 615_000.gePmRange,            FilterType.Spectroscopic)
  /** @group Constructors */ case object RG780            extends GmosSouthFilter("RG780",            "RG780",   "RG780_G0334",                880_000.pm, 780_000.gePmRange,            FilterType.Spectroscopic)
  /** @group Constructors */ case object CaT              extends GmosSouthFilter("CaT",              "CaT",     "CaT_G0333",                  860_000.pm, (780_000,   933_000).pmRange, FilterType.BroadBand)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosSouthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0337 + r_G0326",  630_000.pm, 630_000.gePmRange,            FilterType.Engineering)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosSouthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0338 + r_G0326",  630_000.pm, 630_000.gePmRange,            FilterType.Engineering)
  /** @group Constructors */ case object GPrime_GG455     extends GmosSouthFilter("GPrime_GG455",     "g+GG455", "g_G0325 + GG455_G0329",      506_000.pm, (460_000,   552_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object GPrime_OG515     extends GmosSouthFilter("GPrime_OG515",     "g+OG515", "g_G0325 + OG515_G0330",      536_000.pm, (520_000,   552_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object RPrime_RG610     extends GmosSouthFilter("RPrime_RG610",     "r+RG610", "r_G0326 + RG610_G0331",      657_000.pm, (615_000,   698_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object IPrime_RG780     extends GmosSouthFilter("IPrime_RG780",     "i+RG780", "i_G0327 + RG780_G0334",      819_000.pm, (777_000,   851_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object IPrime_CaT       extends GmosSouthFilter("IPrime_CaT",       "i+CaT",   "i_G0327 + CaT_G0333",        815_000.pm, (780_000,   850_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object ZPrime_CaT       extends GmosSouthFilter("ZPrime_CaT",       "z+Cat",   "z_G0328 + CaT_G0333",        890_000.pm, (848_000,   933_000).pmRange, FilterType.Combination)
  /** @group Constructors */ case object Ha               extends GmosSouthFilter("Ha",               "Ha",      "Ha_G0336",                   656_000.pm, (654_000,   661_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object SII              extends GmosSouthFilter("SII",              "SII",     "SII_G0335",                  672_000.pm, (669_400,   673_700).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HaC              extends GmosSouthFilter("HaC",              "HaC",     "HaC_G0337",                  662_000.pm, (659_000,   665_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OIII             extends GmosSouthFilter("OIII",             "OIII",    "OIII_G0338",                 499_000.pm, (496_500,   501_500).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OIIIC            extends GmosSouthFilter("OIIIC",            "OIIIC",   "OIIIC_G0339",                514_000.pm, (509_000,   519_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HeII             extends GmosSouthFilter("HeII",             "HeII",    "HeII_G0340",                 468_000.pm, (464_000,   472_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object HeIIC            extends GmosSouthFilter("HeIIC",            "HeIIC",   "HeIIC_G0341",                478_000.pm, (474_000,   482_000).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OVI              extends GmosSouthFilter("OVI",              "OVI",     "OVI_G0347",                  684_000.pm, (681_600,   686_500).pmRange, FilterType.NarrowBand)
  /** @group Constructors */ case object OVIC             extends GmosSouthFilter("OVIC",             "OVIC",    "OVI_G0348",                  679_000.pm, (676_100,   680_900).pmRange, FilterType.NarrowBand)

  /** All members of GmosSouthFilter, in canonical order. */
  val all: List[GmosSouthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, RG780, CaT, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_RG780, IPrime_CaT, ZPrime_CaT, Ha, SII, HaC, OIII, OIIIC, HeII, HeIIC, OVI, OVIC)

  /** Acquisition filter options. */
  val acquisition: NonEmptyList[GmosSouthFilter] =
    NonEmptyList.fromListUnsafe(
      List(UPrime, GPrime, RPrime, IPrime, ZPrime)
    )

  /** Select the member of GmosSouthFilter with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthFilter] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthFilter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthFilterEnumerated: Enumerated[GmosSouthFilter] =
    new Enumerated[GmosSouthFilter] {
      def all = GmosSouthFilter.all
      def tag(a: GmosSouthFilter) = a.tag
      override def unsafeFromTag(s: String): GmosSouthFilter =
        GmosSouthFilter.unsafeFromTag(s)
    }

}

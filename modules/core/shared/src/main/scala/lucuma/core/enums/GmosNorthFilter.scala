// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.all._
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import spire.math.Interval

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
  val width: Option[Interval[Wavelength]],
  val filterType: FilterType
) extends Product with Serializable

object GmosNorthFilter {

  /** @group Constructors */ case object GPrime extends GmosNorthFilter("GPrime", "g", "g_G0301", Wavelength.unsafeFromInt(475000), false, Interval(Wavelength.unsafeFromInt(398000), Wavelength.unsafeFromInt(552000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object RPrime extends GmosNorthFilter("RPrime", "r", "r_G0303", Wavelength.unsafeFromInt(630000), false, Interval(Wavelength.unsafeFromInt(562000), Wavelength.unsafeFromInt(698000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object IPrime extends GmosNorthFilter("IPrime", "i", "i_G0302", Wavelength.unsafeFromInt(780000), false, Interval(Wavelength.unsafeFromInt(706000), Wavelength.unsafeFromInt(850000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object ZPrime extends GmosNorthFilter("ZPrime", "z", "z_G0304", Wavelength.unsafeFromInt(925000), false, none, FilterType.BroadBand)
  /** @group Constructors */ case object Z extends GmosNorthFilter("Z", "Z", "Z_G0322", Wavelength.unsafeFromInt(876000), false, Interval(Wavelength.unsafeFromInt(830000), Wavelength.unsafeFromInt(925000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object Y extends GmosNorthFilter("Y", "Y", "Y_G0323", Wavelength.unsafeFromInt(1010000), false, Interval(Wavelength.unsafeFromInt(970000), Wavelength.unsafeFromInt(1010000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object GG455 extends GmosNorthFilter("GG455", "GG455", "GG455_G0305", Wavelength.unsafeFromInt(680000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object OG515 extends GmosNorthFilter("OG515", "OG515", "OG515_G0306", Wavelength.unsafeFromInt(710000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object RG610 extends GmosNorthFilter("RG610", "RG610", "RG610_G0307", Wavelength.unsafeFromInt(750000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object CaT extends GmosNorthFilter("CaT", "CaT", "CaT_G0309", Wavelength.unsafeFromInt(860000), false, Interval(Wavelength.unsafeFromInt(780000), Wavelength.unsafeFromInt(933000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object Ha extends GmosNorthFilter("Ha", "Ha", "Ha_G0310", Wavelength.unsafeFromInt(656000), false, Interval(Wavelength.unsafeFromInt(654000), Wavelength.unsafeFromInt(661000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HaC extends GmosNorthFilter("HaC", "HaC", "HaC_G0311", Wavelength.unsafeFromInt(662000), false, Interval(Wavelength.unsafeFromInt(659000), Wavelength.unsafeFromInt(665000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object DS920 extends GmosNorthFilter("DS920", "DS920", "DS920_G0312", Wavelength.unsafeFromInt(920000), false, Interval(Wavelength.unsafeFromInt(912800), Wavelength.unsafeFromInt(931400)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object SII extends GmosNorthFilter("SII", "SII", "SII_G0317", Wavelength.unsafeFromInt(672000), false, Interval(Wavelength.unsafeFromInt(669400), Wavelength.unsafeFromInt(673700)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIII extends GmosNorthFilter("OIII", "OIII", "OIII_G0318", Wavelength.unsafeFromInt(499000), false, Interval(Wavelength.unsafeFromInt(496500), Wavelength.unsafeFromInt(501500)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIIIC extends GmosNorthFilter("OIIIC", "OIIIC", "OIIIC_G0319", Wavelength.unsafeFromInt(514000), false, Interval(Wavelength.unsafeFromInt(509000), Wavelength.unsafeFromInt(519000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeII extends GmosNorthFilter("HeII", "HeII", "HeII_G0320", Wavelength.unsafeFromInt(468000), false, Interval(Wavelength.unsafeFromInt(464000), Wavelength.unsafeFromInt(472000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeIIC extends GmosNorthFilter("HeIIC", "HeIIC", "HeIIC_G0321", Wavelength.unsafeFromInt(478000), false, Interval(Wavelength.unsafeFromInt(474000), Wavelength.unsafeFromInt(482000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosNorthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0313 + r_G0303", Wavelength.unsafeFromInt(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosNorthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0314 + r_G0303", Wavelength.unsafeFromInt(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object GPrime_GG455 extends GmosNorthFilter("GPrime_GG455", "g+GG455", "g_G0301 + GG455_G0305", Wavelength.unsafeFromInt(506000), false, Interval(Wavelength.unsafeFromInt(460000), Wavelength.unsafeFromInt(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object GPrime_OG515 extends GmosNorthFilter("GPrime_OG515", "g+OG515", "g_G0301 + OG515_G0306", Wavelength.unsafeFromInt(536000), false, Interval(Wavelength.unsafeFromInt(520000), Wavelength.unsafeFromInt(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object RPrime_RG610 extends GmosNorthFilter("RPrime_RG610", "r+RG610", "r_G0303 + RG610_G0307", Wavelength.unsafeFromInt(657000), false, Interval(Wavelength.unsafeFromInt(615000), Wavelength.unsafeFromInt(698000)).some, FilterType.Combination)
  /** @group Constructors */ case object IPrime_CaT extends GmosNorthFilter("IPrime_CaT", "i+CaT", "i_G0302 + CaT_G0309", Wavelength.unsafeFromInt(815000), false, Interval(Wavelength.unsafeFromInt(780000), Wavelength.unsafeFromInt(850000)).some, FilterType.Combination)
  /** @group Constructors */ case object ZPrime_CaT extends GmosNorthFilter("ZPrime_CaT", "z+CaT", "z_G0305 + CaT_G0309", Wavelength.unsafeFromInt(890000), false, Interval(Wavelength.unsafeFromInt(848000), Wavelength.unsafeFromInt(933000)).some, FilterType.Combination)
  /** @group Constructors */ case object UPrime extends GmosNorthFilter("UPrime", "u", "u_G0308", Wavelength.unsafeFromInt(350000), true, Interval(Wavelength.unsafeFromInt(336000), Wavelength.unsafeFromInt(385000)).some, FilterType.BroadBand)

  /** All members of GmosNorthFilter, in canonical order. */
  val all: List[GmosNorthFilter] =
    List(GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, CaT, Ha, HaC, DS920, SII, OIII, OIIIC, HeII, HeIIC, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_CaT, ZPrime_CaT, UPrime)

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

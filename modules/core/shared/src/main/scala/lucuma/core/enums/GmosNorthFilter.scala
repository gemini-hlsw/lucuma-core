// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.data.NonEmptyList
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

  /** @group Constructors */ case object GPrime extends GmosNorthFilter("GPrime", "g", "g_G0301", Wavelength.unsafeFromIntPicometers(475000), false, Interval(Wavelength.unsafeFromIntPicometers(398000), Wavelength.unsafeFromIntPicometers(552000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object RPrime extends GmosNorthFilter("RPrime", "r", "r_G0303", Wavelength.unsafeFromIntPicometers(630000), false, Interval(Wavelength.unsafeFromIntPicometers(562000), Wavelength.unsafeFromIntPicometers(698000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object IPrime extends GmosNorthFilter("IPrime", "i", "i_G0302", Wavelength.unsafeFromIntPicometers(780000), false, Interval(Wavelength.unsafeFromIntPicometers(706000), Wavelength.unsafeFromIntPicometers(850000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object ZPrime extends GmosNorthFilter("ZPrime", "z", "z_G0304", Wavelength.unsafeFromIntPicometers(925000), false, none, FilterType.BroadBand)
  /** @group Constructors */ case object Z extends GmosNorthFilter("Z", "Z", "Z_G0322", Wavelength.unsafeFromIntPicometers(876000), false, Interval(Wavelength.unsafeFromIntPicometers(830000), Wavelength.unsafeFromIntPicometers(925000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object Y extends GmosNorthFilter("Y", "Y", "Y_G0323", Wavelength.unsafeFromIntPicometers(1010000), false, Interval(Wavelength.unsafeFromIntPicometers(970000), Wavelength.unsafeFromIntPicometers(1010000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object GG455 extends GmosNorthFilter("GG455", "GG455", "GG455_G0305", Wavelength.unsafeFromIntPicometers(680000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object OG515 extends GmosNorthFilter("OG515", "OG515", "OG515_G0306", Wavelength.unsafeFromIntPicometers(710000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object RG610 extends GmosNorthFilter("RG610", "RG610", "RG610_G0307", Wavelength.unsafeFromIntPicometers(750000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object CaT extends GmosNorthFilter("CaT", "CaT", "CaT_G0309", Wavelength.unsafeFromIntPicometers(860000), false, Interval(Wavelength.unsafeFromIntPicometers(780000), Wavelength.unsafeFromIntPicometers(933000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object Ha extends GmosNorthFilter("Ha", "Ha", "Ha_G0310", Wavelength.unsafeFromIntPicometers(656000), false, Interval(Wavelength.unsafeFromIntPicometers(654000), Wavelength.unsafeFromIntPicometers(661000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HaC extends GmosNorthFilter("HaC", "HaC", "HaC_G0311", Wavelength.unsafeFromIntPicometers(662000), false, Interval(Wavelength.unsafeFromIntPicometers(659000), Wavelength.unsafeFromIntPicometers(665000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object DS920 extends GmosNorthFilter("DS920", "DS920", "DS920_G0312", Wavelength.unsafeFromIntPicometers(920000), false, Interval(Wavelength.unsafeFromIntPicometers(912800), Wavelength.unsafeFromIntPicometers(931400)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object SII extends GmosNorthFilter("SII", "SII", "SII_G0317", Wavelength.unsafeFromIntPicometers(672000), false, Interval(Wavelength.unsafeFromIntPicometers(669400), Wavelength.unsafeFromIntPicometers(673700)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIII extends GmosNorthFilter("OIII", "OIII", "OIII_G0318", Wavelength.unsafeFromIntPicometers(499000), false, Interval(Wavelength.unsafeFromIntPicometers(496500), Wavelength.unsafeFromIntPicometers(501500)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIIIC extends GmosNorthFilter("OIIIC", "OIIIC", "OIIIC_G0319", Wavelength.unsafeFromIntPicometers(514000), false, Interval(Wavelength.unsafeFromIntPicometers(509000), Wavelength.unsafeFromIntPicometers(519000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeII extends GmosNorthFilter("HeII", "HeII", "HeII_G0320", Wavelength.unsafeFromIntPicometers(468000), false, Interval(Wavelength.unsafeFromIntPicometers(464000), Wavelength.unsafeFromIntPicometers(472000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeIIC extends GmosNorthFilter("HeIIC", "HeIIC", "HeIIC_G0321", Wavelength.unsafeFromIntPicometers(478000), false, Interval(Wavelength.unsafeFromIntPicometers(474000), Wavelength.unsafeFromIntPicometers(482000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosNorthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0313 + r_G0303", Wavelength.unsafeFromIntPicometers(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosNorthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0314 + r_G0303", Wavelength.unsafeFromIntPicometers(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object GPrime_GG455 extends GmosNorthFilter("GPrime_GG455", "g+GG455", "g_G0301 + GG455_G0305", Wavelength.unsafeFromIntPicometers(506000), false, Interval(Wavelength.unsafeFromIntPicometers(460000), Wavelength.unsafeFromIntPicometers(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object GPrime_OG515 extends GmosNorthFilter("GPrime_OG515", "g+OG515", "g_G0301 + OG515_G0306", Wavelength.unsafeFromIntPicometers(536000), false, Interval(Wavelength.unsafeFromIntPicometers(520000), Wavelength.unsafeFromIntPicometers(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object RPrime_RG610 extends GmosNorthFilter("RPrime_RG610", "r+RG610", "r_G0303 + RG610_G0307", Wavelength.unsafeFromIntPicometers(657000), false, Interval(Wavelength.unsafeFromIntPicometers(615000), Wavelength.unsafeFromIntPicometers(698000)).some, FilterType.Combination)
  /** @group Constructors */ case object IPrime_CaT extends GmosNorthFilter("IPrime_CaT", "i+CaT", "i_G0302 + CaT_G0309", Wavelength.unsafeFromIntPicometers(815000), false, Interval(Wavelength.unsafeFromIntPicometers(780000), Wavelength.unsafeFromIntPicometers(850000)).some, FilterType.Combination)
  /** @group Constructors */ case object ZPrime_CaT extends GmosNorthFilter("ZPrime_CaT", "z+CaT", "z_G0305 + CaT_G0309", Wavelength.unsafeFromIntPicometers(890000), false, Interval(Wavelength.unsafeFromIntPicometers(848000), Wavelength.unsafeFromIntPicometers(933000)).some, FilterType.Combination)
  /** @group Constructors */ case object UPrime extends GmosNorthFilter("UPrime", "u", "u_G0308", Wavelength.unsafeFromIntPicometers(350000), true, Interval(Wavelength.unsafeFromIntPicometers(336000), Wavelength.unsafeFromIntPicometers(385000)).some, FilterType.BroadBand)

  /** All members of GmosNorthFilter, in canonical order. */
  val all: List[GmosNorthFilter] =
    List(GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, CaT, Ha, HaC, DS920, SII, OIII, OIIIC, HeII, HeIIC, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_CaT, ZPrime_CaT, UPrime)

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

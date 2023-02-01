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
 * Enumerated type for GMOS South filters.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val obsolete: Boolean,
  val width: Option[Interval[Wavelength]],
  val filterType: FilterType
) extends Product with Serializable

object GmosSouthFilter {

  /** @group Constructors */ case object UPrime extends GmosSouthFilter("UPrime", "u", "u_G0332", Wavelength.unsafeFromIntPicometers(350000), false, Interval(Wavelength.unsafeFromIntPicometers(336000), Wavelength.unsafeFromIntPicometers(385000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object GPrime extends GmosSouthFilter("GPrime", "g", "g_G0325", Wavelength.unsafeFromIntPicometers(475000), false, Interval(Wavelength.unsafeFromIntPicometers(398000), Wavelength.unsafeFromIntPicometers(552000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object RPrime extends GmosSouthFilter("RPrime", "r", "r_G0326", Wavelength.unsafeFromIntPicometers(630000), false, Interval(Wavelength.unsafeFromIntPicometers(562000), Wavelength.unsafeFromIntPicometers(698000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object IPrime extends GmosSouthFilter("IPrime", "i", "i_G0327", Wavelength.unsafeFromIntPicometers(780000), false, Interval(Wavelength.unsafeFromIntPicometers(706000), Wavelength.unsafeFromIntPicometers(850000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object ZPrime extends GmosSouthFilter("ZPrime", "z", "z_G0328", Wavelength.unsafeFromIntPicometers(925000), false, none, FilterType.BroadBand)
  /** @group Constructors */ case object Z extends GmosSouthFilter("Z", "Z", "Z_G0343", Wavelength.unsafeFromIntPicometers(876000), false, Interval(Wavelength.unsafeFromIntPicometers(830000), Wavelength.unsafeFromIntPicometers(925000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object Y extends GmosSouthFilter("Y", "Y", "Y_G0344", Wavelength.unsafeFromIntPicometers(1010000), false, Interval(Wavelength.unsafeFromIntPicometers(970000), Wavelength.unsafeFromIntPicometers(1010000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object GG455 extends GmosSouthFilter("GG455", "GG455", "GG455_G0329", Wavelength.unsafeFromIntPicometers(680000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object OG515 extends GmosSouthFilter("OG515", "OG515", "OG515_G0330", Wavelength.unsafeFromIntPicometers(710000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object RG610 extends GmosSouthFilter("RG610", "RG610", "RG610_G0331", Wavelength.unsafeFromIntPicometers(750000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object RG780 extends GmosSouthFilter("RG780", "RG780", "RG780_G0334", Wavelength.unsafeFromIntPicometers(850000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object CaT extends GmosSouthFilter("CaT", "CaT", "CaT_G0333", Wavelength.unsafeFromIntPicometers(860000), false, Interval(Wavelength.unsafeFromIntPicometers(780000), Wavelength.unsafeFromIntPicometers(933000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosSouthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0337 + r_G0326", Wavelength.unsafeFromIntPicometers(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosSouthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0338 + r_G0326", Wavelength.unsafeFromIntPicometers(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object GPrime_GG455 extends GmosSouthFilter("GPrime_GG455", "g+GG455", "g_G0325 + GG455_G0329", Wavelength.unsafeFromIntPicometers(506000), false, Interval(Wavelength.unsafeFromIntPicometers(460000), Wavelength.unsafeFromIntPicometers(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object GPrime_OG515 extends GmosSouthFilter("GPrime_OG515", "g+OG515", "g_G0325 + OG515_G0330", Wavelength.unsafeFromIntPicometers(536000), false, Interval(Wavelength.unsafeFromIntPicometers(520000), Wavelength.unsafeFromIntPicometers(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object RPrime_RG610 extends GmosSouthFilter("RPrime_RG610", "r+RG610", "r_G0326 + RG610_G0331", Wavelength.unsafeFromIntPicometers(657000), false, Interval(Wavelength.unsafeFromIntPicometers(615000), Wavelength.unsafeFromIntPicometers(698000)).some, FilterType.Combination)
  /** @group Constructors */ case object IPrime_RG780 extends GmosSouthFilter("IPrime_RG780", "i+RG780", "i_G0327 + RG780_G0334", Wavelength.unsafeFromIntPicometers(819000), false, Interval(Wavelength.unsafeFromIntPicometers(777000), Wavelength.unsafeFromIntPicometers(851000)).some, FilterType.Combination)
  /** @group Constructors */ case object IPrime_CaT extends GmosSouthFilter("IPrime_CaT", "i+CaT", "i_G0327 + CaT_G0333", Wavelength.unsafeFromIntPicometers(815000), false, Interval(Wavelength.unsafeFromIntPicometers(780000), Wavelength.unsafeFromIntPicometers(850000)).some, FilterType.Combination)
  /** @group Constructors */ case object ZPrime_CaT extends GmosSouthFilter("ZPrime_CaT", "z+Cat", "z_G0328 + CaT_G0333", Wavelength.unsafeFromIntPicometers(890000), false, Interval(Wavelength.unsafeFromIntPicometers(848000), Wavelength.unsafeFromIntPicometers(933000)).some, FilterType.Combination)
  /** @group Constructors */ case object Ha extends GmosSouthFilter("Ha", "Ha", "Ha_G0336", Wavelength.unsafeFromIntPicometers(656000), false, Interval(Wavelength.unsafeFromIntPicometers(654000), Wavelength.unsafeFromIntPicometers(661000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object SII extends GmosSouthFilter("SII", "SII", "SII_G0335", Wavelength.unsafeFromIntPicometers(672000), false, Interval(Wavelength.unsafeFromIntPicometers(669400), Wavelength.unsafeFromIntPicometers(673700)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HaC extends GmosSouthFilter("HaC", "HaC", "HaC_G0337", Wavelength.unsafeFromIntPicometers(662000), false, Interval(Wavelength.unsafeFromIntPicometers(659000), Wavelength.unsafeFromIntPicometers(665000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIII extends GmosSouthFilter("OIII", "OIII", "OIII_G0338", Wavelength.unsafeFromIntPicometers(499000), false, Interval(Wavelength.unsafeFromIntPicometers(496500), Wavelength.unsafeFromIntPicometers(501500)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIIIC extends GmosSouthFilter("OIIIC", "OIIIC", "OIIIC_G0339", Wavelength.unsafeFromIntPicometers(514000), false, Interval(Wavelength.unsafeFromIntPicometers(509000), Wavelength.unsafeFromIntPicometers(519000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeII extends GmosSouthFilter("HeII", "HeII", "HeII_G0340", Wavelength.unsafeFromIntPicometers(468000), false, Interval(Wavelength.unsafeFromIntPicometers(464000), Wavelength.unsafeFromIntPicometers(472000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeIIC extends GmosSouthFilter("HeIIC", "HeIIC", "HeIIC_G0341", Wavelength.unsafeFromIntPicometers(478000), false, Interval(Wavelength.unsafeFromIntPicometers(474000), Wavelength.unsafeFromIntPicometers(482000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object Lya395 extends GmosSouthFilter("Lya395", "Lya395", "Lya395_G0342", Wavelength.unsafeFromIntPicometers(396000), true, none, FilterType.NarrowBand)

  /** All members of GmosSouthFilter, in canonical order. */
  val all: List[GmosSouthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, RG780, CaT, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_RG780, IPrime_CaT, ZPrime_CaT, Ha, SII, HaC, OIII, OIIIC, HeII, HeIIC, Lya395)

  /** Acquisition filter options. */
  val acquisition: NonEmptyList[GmosSouthFilter] =
    NonEmptyList.of(UPrime, GPrime, RPrime, IPrime, ZPrime)

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

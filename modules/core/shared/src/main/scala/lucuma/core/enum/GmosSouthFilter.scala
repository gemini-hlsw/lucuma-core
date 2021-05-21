// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum

import cats.syntax.all._
import lucuma.core.util.Enumerated
import lucuma.core.math.Wavelength
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

  /** @group Constructors */ case object UPrime extends GmosSouthFilter("UPrime", "u", "u_G0332", Wavelength.unsafeFromInt(350000), false, Interval(Wavelength.unsafeFromInt(336000), Wavelength.unsafeFromInt(385000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object GPrime extends GmosSouthFilter("GPrime", "g", "g_G0325", Wavelength.unsafeFromInt(475000), false, Interval(Wavelength.unsafeFromInt(398000), Wavelength.unsafeFromInt(552000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object RPrime extends GmosSouthFilter("RPrime", "r", "r_G0326", Wavelength.unsafeFromInt(630000), false, Interval(Wavelength.unsafeFromInt(562000), Wavelength.unsafeFromInt(698000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object IPrime extends GmosSouthFilter("IPrime", "i", "i_G0327", Wavelength.unsafeFromInt(780000), false, Interval(Wavelength.unsafeFromInt(706000), Wavelength.unsafeFromInt(850000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object ZPrime extends GmosSouthFilter("ZPrime", "z", "z_G0328", Wavelength.unsafeFromInt(925000), false, none, FilterType.BroadBand)
  /** @group Constructors */ case object Z extends GmosSouthFilter("Z", "Z", "Z_G0343", Wavelength.unsafeFromInt(876000), false, Interval(Wavelength.unsafeFromInt(830000), Wavelength.unsafeFromInt(925000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object Y extends GmosSouthFilter("Y", "Y", "Y_G0344", Wavelength.unsafeFromInt(1010000), false, Interval(Wavelength.unsafeFromInt(970000), Wavelength.unsafeFromInt(1010000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object GG455 extends GmosSouthFilter("GG455", "GG455", "GG455_G0329", Wavelength.unsafeFromInt(680000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object OG515 extends GmosSouthFilter("OG515", "OG515", "OG515_G0330", Wavelength.unsafeFromInt(710000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object RG610 extends GmosSouthFilter("RG610", "RG610", "RG610_G0331", Wavelength.unsafeFromInt(750000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object RG780 extends GmosSouthFilter("RG780", "RG780", "RG780_G0334", Wavelength.unsafeFromInt(850000), false, none, FilterType.Spectroscopic)
  /** @group Constructors */ case object CaT extends GmosSouthFilter("CaT", "CaT", "CaT_G0333", Wavelength.unsafeFromInt(860000), false, Interval(Wavelength.unsafeFromInt(780000), Wavelength.unsafeFromInt(933000)).some, FilterType.BroadBand)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosSouthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0337 + r_G0326", Wavelength.unsafeFromInt(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosSouthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0338 + r_G0326", Wavelength.unsafeFromInt(630000), false, none, FilterType.Engineering)
  /** @group Constructors */ case object GPrime_GG455 extends GmosSouthFilter("GPrime_GG455", "g+GG455", "g_G0325 + GG455_G0329", Wavelength.unsafeFromInt(506000), false, Interval(Wavelength.unsafeFromInt(460000), Wavelength.unsafeFromInt(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object GPrime_OG515 extends GmosSouthFilter("GPrime_OG515", "g+OG515", "g_G0325 + OG515_G0330", Wavelength.unsafeFromInt(536000), false, Interval(Wavelength.unsafeFromInt(520000), Wavelength.unsafeFromInt(552000)).some, FilterType.Combination)
  /** @group Constructors */ case object RPrime_RG610 extends GmosSouthFilter("RPrime_RG610", "r+RG610", "r_G0326 + RG610_G0331", Wavelength.unsafeFromInt(657000), false, Interval(Wavelength.unsafeFromInt(615000), Wavelength.unsafeFromInt(698000)).some, FilterType.Combination)
  /** @group Constructors */ case object IPrime_RG780 extends GmosSouthFilter("IPrime_RG780", "i+RG780", "i_G0327 + RG780_G0334", Wavelength.unsafeFromInt(819000), false, Interval(Wavelength.unsafeFromInt(777000), Wavelength.unsafeFromInt(851000)).some, FilterType.Combination)
  /** @group Constructors */ case object IPrime_CaT extends GmosSouthFilter("IPrime_CaT", "i+CaT", "i_G0327 + CaT_G0333", Wavelength.unsafeFromInt(815000), false, Interval(Wavelength.unsafeFromInt(780000), Wavelength.unsafeFromInt(850000)).some, FilterType.Combination)
  /** @group Constructors */ case object ZPrime_CaT extends GmosSouthFilter("ZPrime_CaT", "z+Cat", "z_G0328 + CaT_G0333", Wavelength.unsafeFromInt(890000), false, Interval(Wavelength.unsafeFromInt(848000), Wavelength.unsafeFromInt(933000)).some, FilterType.Combination)
  /** @group Constructors */ case object Ha extends GmosSouthFilter("Ha", "Ha", "Ha_G0336", Wavelength.unsafeFromInt(656000), false, Interval(Wavelength.unsafeFromInt(654000), Wavelength.unsafeFromInt(661000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object SII extends GmosSouthFilter("SII", "SII", "SII_G0335", Wavelength.unsafeFromInt(672000), false, Interval(Wavelength.unsafeFromInt(669400), Wavelength.unsafeFromInt(673700)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HaC extends GmosSouthFilter("HaC", "HaC", "HaC_G0337", Wavelength.unsafeFromInt(662000), false, Interval(Wavelength.unsafeFromInt(659000), Wavelength.unsafeFromInt(665000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIII extends GmosSouthFilter("OIII", "OIII", "OIII_G0338", Wavelength.unsafeFromInt(499000), false, Interval(Wavelength.unsafeFromInt(496500), Wavelength.unsafeFromInt(501500)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object OIIIC extends GmosSouthFilter("OIIIC", "OIIIC", "OIIIC_G0339", Wavelength.unsafeFromInt(514000), false, Interval(Wavelength.unsafeFromInt(509000), Wavelength.unsafeFromInt(519000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeII extends GmosSouthFilter("HeII", "HeII", "HeII_G0340", Wavelength.unsafeFromInt(468000), false, Interval(Wavelength.unsafeFromInt(464000), Wavelength.unsafeFromInt(472000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object HeIIC extends GmosSouthFilter("HeIIC", "HeIIC", "HeIIC_G0341", Wavelength.unsafeFromInt(478000), false, Interval(Wavelength.unsafeFromInt(474000), Wavelength.unsafeFromInt(482000)).some, FilterType.NarrowBand)
  /** @group Constructors */ case object Lya395 extends GmosSouthFilter("Lya395", "Lya395", "Lya395_G0342", Wavelength.unsafeFromInt(396000), true, none, FilterType.NarrowBand)

  /** All members of GmosSouthFilter, in canonical order. */
  val all: List[GmosSouthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, RG780, CaT, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_RG780, IPrime_CaT, ZPrime_CaT, Ha, SII, HaC, OIII, OIIIC, HeII, HeIIC, Lya395)

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

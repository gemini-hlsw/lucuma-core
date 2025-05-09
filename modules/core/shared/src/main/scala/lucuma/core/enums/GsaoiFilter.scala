// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan


/**
 * Enumerated type for GSAOI Filter.
 * @group Enumerations
 */
sealed abstract class GsaoiFilter(
  val tag:                  String,
  val shortName:            String,
  val longName:             String,
  val wavelength:           Wavelength,
  val readMode:             GsaoiReadMode,
  val exposureTime5050:     TimeSpan,
  val exposureTimeHalfWell: TimeSpan,
  val band:                 Option[Band]
) extends Product
    with Serializable

object GsaoiFilter {

  /** @group Constructors */
  case object Z
      extends GsaoiFilter(
        "Z",
        "Z",
        "Z (1.015 um)",
        Wavelength.unsafeFromIntPicometers(1020000),
        GsaoiReadMode.Faint,
        26000.msTimeSpan,
        4619.secTimeSpan,
        Some(Band.J)
      )

  /** @group Constructors */
  case object HeI
      extends GsaoiFilter(
        "HeI",
        "HeI",
        "HeI (1.083 um)",
        Wavelength.unsafeFromIntPicometers(1080000),
        GsaoiReadMode.VeryFaint,
        72600.msTimeSpan,
        21792.secTimeSpan,
        Some(Band.J)
      )

  /** @group Constructors */
  case object PaGamma
      extends GsaoiFilter(
        "PaGamma",
        "Pagma",
        "Pa(gamma) (1.094 um)",
        Wavelength.unsafeFromIntPicometers(1090000),
        GsaoiReadMode.VeryFaint,
        122000.msTimeSpan,
        36585.secTimeSpan,
        Some(Band.J)
      )

  /** @group Constructors */
  case object JContinuum
      extends GsaoiFilter(
        "JContinuum",
        "Jcont",
        "J-continuum (1.207 um)",
        Wavelength.unsafeFromIntPicometers(1210000),
        GsaoiReadMode.VeryFaint,
        32600.msTimeSpan,
        9793.secTimeSpan,
        Some(Band.J)
      )

  /** @group Constructors */
  case object J
      extends GsaoiFilter(
        "J",
        "J",
        "J (1.250 um)",
        Wavelength.unsafeFromIntPicometers(1250000),
        GsaoiReadMode.Faint,
        5700.msTimeSpan,
        1004.secTimeSpan,
        Some(Band.J)
      )

  /** @group Constructors */
  case object H
      extends GsaoiFilter(
        "H",
        "H",
        "H (1.635 um)",
        Wavelength.unsafeFromIntPicometers(1640000),
        GsaoiReadMode.Bright,
        12000.msTimeSpan,
        460.secTimeSpan,
        Some(Band.H)
      )

  /** @group Constructors */
  case object PaBeta
      extends GsaoiFilter(
        "PaBeta",
        "Pabeta",
        "Pa(beta) (1.282 um)",
        Wavelength.unsafeFromIntPicometers(1280000),
        GsaoiReadMode.Faint,
        21800.msTimeSpan,
        3879.secTimeSpan,
        Some(Band.J)
      )

  /** @group Constructors */
  case object HContinuum
      extends GsaoiFilter(
        "HContinuum",
        "Hcont",
        "H-continuum (1.570 um)",
        Wavelength.unsafeFromIntPicometers(1570000),
        GsaoiReadMode.Faint,
        31200.msTimeSpan,
        5545.secTimeSpan,
        Some(Band.H)
      )

  /** @group Constructors */
  case object CH4Short
      extends GsaoiFilter(
        "CH4Short",
        "CH4short",
        "CH4(short) (1.580 um)",
        Wavelength.unsafeFromIntPicometers(1580000),
        GsaoiReadMode.Faint,
        6600.msTimeSpan,
        1174.secTimeSpan,
        Some(Band.H)
      )

  /** @group Constructors */
  case object FeII
      extends GsaoiFilter(
        "FeII",
        "FeII1644",
        "[Fe II] (1.644 um)",
        Wavelength.unsafeFromIntPicometers(1640000),
        GsaoiReadMode.Faint,
        24900.msTimeSpan,
        4416.secTimeSpan,
        Some(Band.H)
      )

  /** @group Constructors */
  case object CH4Long
      extends GsaoiFilter(
        "CH4Long",
        "CH4long",
        "CH4(long) (1.690 um)",
        Wavelength.unsafeFromIntPicometers(1690000),
        GsaoiReadMode.Faint,
        6800.msTimeSpan,
        1202.secTimeSpan,
        Some(Band.H)
      )

  /** @group Constructors */
  case object H20Ice
      extends GsaoiFilter(
        "H20Ice",
        "H20ice",
        "H20 ice (2.000 um)",
        Wavelength.unsafeFromIntPicometers(2000000),
        GsaoiReadMode.Faint,
        19100.msTimeSpan,
        3395.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object HeI2p2s
      extends GsaoiFilter(
        "HeI2p2s",
        "HeI2p2s",
        "HeI (2p2s) (2.058 um)",
        Wavelength.unsafeFromIntPicometers(2060000),
        GsaoiReadMode.Faint,
        28300.msTimeSpan,
        5032.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object KContinuum1
      extends GsaoiFilter(
        "KContinuum1",
        "Kcontshrt",
        "Ks-continuum (2.093 um)",
        Wavelength.unsafeFromIntPicometers(2090000),
        GsaoiReadMode.Faint,
        7800.msTimeSpan,
        6069.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object BrGamma
      extends GsaoiFilter(
        "BrGamma",
        "Brgma",
        "Br(gamma) (2.166 um)",
        Wavelength.unsafeFromIntPicometers(2170000),
        GsaoiReadMode.Faint,
        31000.msTimeSpan,
        5496.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object KContinuum2
      extends GsaoiFilter(
        "KContinuum2",
        "Kcontlong",
        "Kl-continuum (2.270 um)",
        Wavelength.unsafeFromIntPicometers(2270000),
        GsaoiReadMode.Faint,
        33300.msTimeSpan,
        5911.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object KPrime
      extends GsaoiFilter(
        "KPrime",
        "Kprime",
        "K(prime) (2.120 um)",
        Wavelength.unsafeFromIntPicometers(2120000),
        GsaoiReadMode.Bright,
        14800.msTimeSpan,
        566.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object H2_1_0_S_1
      extends GsaoiFilter(
        "H2_1_0_S_1",
        "H2(1-0)",
        "H2 1-0 S(1) (2.122 um)",
        Wavelength.unsafeFromIntPicometers(2120000),
        GsaoiReadMode.Faint,
        27500.msTimeSpan,
        5400.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object KShort
      extends GsaoiFilter(
        "KShort",
        "Kshort",
        "K(short) (2.150 um)",
        Wavelength.unsafeFromIntPicometers(2150000),
        GsaoiReadMode.Bright,
        14400.msTimeSpan,
        551.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object K
      extends GsaoiFilter(
        "K",
        "K",
        "K (2.200 um)",
        Wavelength.unsafeFromIntPicometers(2200000),
        GsaoiReadMode.Bright,
        12300.msTimeSpan,
        470.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object H2_2_1_S_1
      extends GsaoiFilter(
        "H2_2_1_S_1",
        "H2(2-1)",
        "H2 2-1 S(1) (2.248 um)",
        Wavelength.unsafeFromIntPicometers(2250000),
        GsaoiReadMode.Faint,
        32600.msTimeSpan,
        5784.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object CO
      extends GsaoiFilter(
        "CO",
        "CO2360",
        "CO (2.360 um)",
        Wavelength.unsafeFromIntPicometers(2360000),
        GsaoiReadMode.Faint,
        7700.msTimeSpan,
        1370.secTimeSpan,
        Some(Band.K)
      )

  /** @group Constructors */
  case object Diffuser1
      extends GsaoiFilter(
        "Diffuser1",
        "Diffuser1",
        "Diffuser1",
        Wavelength.unsafeFromIntPicometers(0),
        GsaoiReadMode.Bright,
        0.msTimeSpan,
        0.secTimeSpan,
        Option.empty[Band]
      )

  /** @group Constructors */
  case object Diffuser2
      extends GsaoiFilter(
        "Diffuser2",
        "Diffuser2",
        "Diffuser2",
        Wavelength.unsafeFromIntPicometers(0),
        GsaoiReadMode.Bright,
        0.msTimeSpan,
        0.secTimeSpan,
        Option.empty[Band]
      )

  /** @group Constructors */
  case object Blocked
      extends GsaoiFilter(
        "Blocked",
        "Blocked",
        "Blocked",
        Wavelength.unsafeFromIntPicometers(0),
        GsaoiReadMode.Bright,
        0.msTimeSpan,
        0.secTimeSpan,
        Option.empty[Band]
      )

  /** All members of GsaoiFilter, in canonical order. */
  val all: List[GsaoiFilter] =
    List(
      Z,
      HeI,
      PaGamma,
      JContinuum,
      J,
      H,
      PaBeta,
      HContinuum,
      CH4Short,
      FeII,
      CH4Long,
      H20Ice,
      HeI2p2s,
      KContinuum1,
      BrGamma,
      KContinuum2,
      KPrime,
      H2_1_0_S_1,
      KShort,
      K,
      H2_2_1_S_1,
      CO,
      Diffuser1,
      Diffuser2,
      Blocked
    )

  /** Select the member of GsaoiFilter with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiFilter] =
    all.find(_.tag === s)

  /** Select the member of GsaoiFilter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GsaoiFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiFilterEnumerated: Enumerated[GsaoiFilter] =
    new Enumerated[GsaoiFilter] {
      def all                                            = GsaoiFilter.all
      def tag(a: GsaoiFilter)                            = a.tag
      override def unsafeFromTag(s: String): GsaoiFilter =
        GsaoiFilter.unsafeFromTag(s)
    }

}

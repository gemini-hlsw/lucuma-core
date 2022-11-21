// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.Order
import cats.syntax.all._
import eu.timepit.refined.auto._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for wavelength band.
 * @group Enumerations
 */
sealed abstract class Band(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val start:     Wavelength,
  val center:    Wavelength,
  val end:       Wavelength
) extends Product
    with Serializable { self =>
  require(center >= start)
  require(end >= center)

  type DefaultIntegratedUnits
  type DefaultSurfaceUnits

  // These implicits here allow resolving the default units at the type level, allowing use of
  // `defaultUnits[T]`; and also in runtime (used eg when parsing) by using `import band._`.
  implicit def defaultIntegrated: Band.DefaultUnits[self.type, Integrated]
  implicit def defaultSurface: Band.DefaultUnits[self.type, Surface]

  def defaultUnits[T](implicit ev: Band.DefaultUnits[self.type, T]): Units Of Brightness[T] =
    ev.units
}

sealed abstract class BandWithDefaultUnits[DI, DS](
  tag:       String,
  shortName: String,
  longName:  String,
  start:     Wavelength,
  center:    Wavelength,
  end:       Wavelength
)(implicit
  taggedI:   TaggedUnit[DI, Brightness[Integrated]],
  taggedS:   TaggedUnit[DS, Brightness[Surface]]
) extends Band(tag, shortName, longName, start, center, end) { self =>
  type DefaultIntegratedUnits = DI
  type DefaultSurfaceUnits    = DS

  implicit val defaultIntegrated: Band.DefaultUnits[self.type, Integrated] =
    Band.DefaultUnits[self.type, Integrated, DI]

  implicit val defaultSurface: Band.DefaultUnits[self.type, Surface] =
    Band.DefaultUnits[self.type, Surface, DS]
}

object Band {

  class DefaultUnits[B, T](val units: Units Of Brightness[T])
  object DefaultUnits {
    // Declare `U` as the default unit for band `B` and brightness type `T` (Integrated or Surface).
    def apply[B, T, U](implicit tagged: TaggedUnit[U, Brightness[T]]) =
      new DefaultUnits[B, T](tagged.unit)
  }

  /** @group Constructors */
  case object SloanU
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanU",
        "u",
        "UV",
        Wavelength(333000),
        Wavelength(356000),
        Wavelength(379000)
      )

  /** @group Constructors */
  case object SloanG
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanG",
        "g",
        "Green",
        Wavelength(433000),
        Wavelength(483000),
        Wavelength(533000)
      )

  /** @group Constructors */
  case object SloanR
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanR",
        "r",
        "Red",
        Wavelength(578000),
        Wavelength(626000),
        Wavelength(674000)
      )

  /** @group Constructors */
  case object SloanI
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanI",
        "i",
        "Far red",
        Wavelength(714000),
        Wavelength(767000),
        Wavelength(820000)
      )

  /** @group Constructors */
  case object SloanZ
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanZ",
        "z",
        "Near infrared",
        Wavelength(847000),
        Wavelength(910000),
        Wavelength(973000)
      )

  /** @group Constructors */
  case object U
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "U",
        "U",
        "Ultraviolet",
        Wavelength(322000),
        Wavelength(360000),
        Wavelength(398000)
      )

  /** @group Constructors */
  case object B
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "B",
        "B",
        "Blue",
        Wavelength(395000),
        Wavelength(440000),
        Wavelength(485000)
      )

  /** @group Constructors */
  case object V
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "V",
        "V",
        "Visual",
        Wavelength(507000),
        Wavelength(550000),
        Wavelength(593000)
      )

  /** @group Constructors */
  case object R
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "R",
        "R",
        "Red",
        Wavelength(620000),
        Wavelength(670000),
        Wavelength(720000)
      )

  /** @group Constructors */
  case object I
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "I",
        "I",
        "Infrared",
        Wavelength(820000),
        Wavelength(870000),
        Wavelength(920000)
      )

  /** @group Constructors */
  case object Y
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Y",
        "Y",
        "Y",
        Wavelength(960000),
        Wavelength(1020000),
        Wavelength(1080000)
      )

  /** @group Constructors */
  case object J
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "J",
        "J",
        "J",
        Wavelength(1130000),
        Wavelength(1250000),
        Wavelength(1370000)
      )

  /** @group Constructors */
  case object H
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "H",
        "H",
        "H",
        Wavelength(1500000),
        Wavelength(1650000),
        Wavelength(1800000)
      )

  /** @group Constructors */
  case object K
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "K",
        "K",
        "K",
        Wavelength(1995000),
        Wavelength(2200000),
        Wavelength(2405000)
      )

  /** @group Constructors */
  case object L
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "L",
        "L",
        "L",
        Wavelength(3410000),
        Wavelength(3760000),
        Wavelength(4110000)
      )

  /** @group Constructors */
  case object M
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "M",
        "M",
        "M",
        Wavelength(4650000),
        Wavelength(4770000),
        Wavelength(4890000)
      )

  /** @group Constructors */
  case object N
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "N",
        "N",
        "N",
        Wavelength(7855000),
        Wavelength(10470000),
        Wavelength(13085000)
      )

  /** @group Constructors */
  case object Q
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Q",
        "Q",
        "Q",
        Wavelength(19305000),
        Wavelength(20130000),
        Wavelength(20955000)
      )

  /** @group Constructors */
  case object Ap
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Ap",
        "AP",
        "Apparent",
        V.start,
        V.center,
        V.end
      )

  /** @group Constructors */
  case object Gaia
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Gaia",
        "G",
        "Gaia Pass Band",
        Wavelength(330000),
        Wavelength(641000),
        Wavelength(1037000)
      )

  /** @group Constructors */
  case object GaiaBP
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "GaiaBP",
        "G_BP",
        "Gaia Blue Pass Band",
        Wavelength(328000),
        Wavelength(513000),
        Wavelength(671000)
      )

  /** @group Constructors */
  case object GaiaRP
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "GaiaRP",
        "G_RP",
        "Gaia Red Pass Band",
        Wavelength(626000),
        Wavelength(778000),
        Wavelength(1051000)
      )

  /** All members of Band, in canonical order. */
  val all: List[Band] =
    List(SloanU, SloanG, SloanR, SloanI, SloanZ, U, B, V, R, I, Y, J, H, K, L, M, N, Q, Ap, Gaia, GaiaBP, GaiaRP)

  /** Select the member of Band with the given tag, if any. */
  def fromTag(s: String): Option[Band] =
    all.find(_.tag === s)

  /** Select the member of Band with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): Band =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"Band: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val BandEnumerated: Enumerated[Band] =
    new Enumerated[Band] {
      def all                                     = Band.all
      def tag(a: Band)                            = a.tag
      override def unsafeFromTag(s: String): Band =
        Band.unsafeFromTag(s)
    }

  /** @group Typeclass Instances */
  val BandWavelengthOrder: Order[Band] =
    Order.by(_.center)

  /** @group Typeclass Instances */
  implicit val BandOrdering: Ordering[Band] =
    BandEnumerated.toOrdering

}

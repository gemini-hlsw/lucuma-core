// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum

import cats.Order
import cats.syntax.eq._
import coulomb._
import coulomb.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.units.Nanometer
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import spire.std.any._
import spire.syntax.all._

/**
 * Enumerated type for wavelength band.
 * @group Enumerations
 */
sealed abstract class Band(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val center:    Wavelength,
  val width:     Quantity[PosInt, Nanometer]
) extends Product
    with Serializable { self =>
  require(center.toPicometers.value >= width.value / 2)

  def end: Wavelength = Wavelength(
    center.toPicometers + (width.value.value / 2).withRefinedUnit[Positive, Nanometer]
  )

  // The require above lets ensure we can do this unsafe refine conversion
  import coulomb.refined.policy.unsoundRefinedConversions._
  def start: Wavelength = Wavelength(
    center.toPicometers - (width.value.value / 2).withRefinedUnit[Positive, Nanometer]
  )

  type DefaultIntegratedUnits
  type DefaultSurfaceUnits

  // These implicits here allow resolving the default units at the type level, where the brightness
  // type is a type parameter (used in BandBrightness constructors);  and also in runtime (used eg when parsing)
  // by using `import band._`.
  implicit def defaultIntegrated: Band.DefaultUnit[self.type, Integrated]
  implicit def defaultSurface: Band.DefaultUnit[self.type, Surface]

  def defaultUnit[T](implicit ev: Band.DefaultUnit[self.type, T]): Units Of Brightness[T] =
    ev.units
}

sealed abstract class BandWithDefaultUnits[DI, DS](
  tag:       String,
  shortName: String,
  longName:  String,
  center:    Wavelength,
  width:     Quantity[PosInt, Nanometer]
)(implicit
  taggedI:   TaggedUnit[DI, Brightness[Integrated]],
  taggedS:   TaggedUnit[DS, Brightness[Surface]]
) extends Band(tag, shortName, longName, center, width) { self =>
  type DefaultIntegratedUnits = DI
  type DefaultSurfaceUnits    = DS

  implicit val defaultIntegrated: Band.DefaultUnit[self.type, Integrated] =
    Band.DefaultUnit[self.type, Integrated, DI]

  implicit val defaultSurface: Band.DefaultUnit[self.type, Surface] =
    Band.DefaultUnit[self.type, Surface, DS]
}

object Band {

  class DefaultUnit[B, T](val units: Units Of Brightness[T])
  object DefaultUnit {
    // Declare `U` as the default unit for band `B` and brightness type `T` (Integrated or Surface).
    def apply[B, T, U](implicit tagged: TaggedUnit[U, Brightness[T]]) =
      new DefaultUnit[B, T](tagged.unit)
  }

  /** @group Constructors */
  case object SloanU
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanU",
        "u",
        "UV",
        Wavelength(356000),
        46.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object SloanG
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanG",
        "g",
        "Green",
        Wavelength(483000),
        99.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object SloanR
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanR",
        "r",
        "Red",
        Wavelength(626000),
        96.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object SloanI
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanI",
        "i",
        "Far red",
        Wavelength(767000),
        106.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object SloanZ
      extends BandWithDefaultUnits[ABMagnitude, ABMagnitudePerArcsec2](
        "SloanZ",
        "z",
        "Near infrared",
        Wavelength(910000),
        125.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object U
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "U",
        "U",
        "Ultraviolet",
        Wavelength(360000),
        75.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object B
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "B",
        "B",
        "Blue",
        Wavelength(440000),
        90.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object V
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "V",
        "V",
        "Visual",
        Wavelength(550000),
        85.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Uc
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Uc",
        "UC",
        "UCAC",
        Wavelength(610000),
        63.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object R
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "R",
        "R",
        "Red",
        Wavelength(670000),
        100.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object I
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "I",
        "I",
        "Infrared",
        Wavelength(870000),
        100.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Y
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Y",
        "Y",
        "Y",
        Wavelength(1020000),
        120.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object J
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "J",
        "J",
        "J",
        Wavelength(1250000),
        240.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object H
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "H",
        "H",
        "H",
        Wavelength(1650000),
        300.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object K
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "K",
        "K",
        "K",
        Wavelength(2200000),
        410.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object L
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "L",
        "L",
        "L",
        Wavelength(3760000),
        700.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object M
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "M",
        "M",
        "M",
        Wavelength(4770000),
        240.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object N
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "N",
        "N",
        "N",
        Wavelength(10470000),
        5230.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Q
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Q",
        "Q",
        "Q",
        Wavelength(20130000),
        1650.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Ap
      extends BandWithDefaultUnits[VegaMagnitude, VegaMagnitudePerArcsec2](
        "Ap",
        "AP",
        "Apparent",
        Wavelength(550000),
        85.withRefinedUnit[Positive, Nanometer]
      )

  /** All members of Band, in canonical order. */
  val all: List[Band] =
    List(SloanU, SloanG, SloanR, SloanI, SloanZ, U, B, V, Uc, R, I, Y, J, H, K, L, M, N, Q, Ap)

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

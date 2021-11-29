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
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Nanometer
import lucuma.core.util.Enumerated
import spire.std.any._
import spire.syntax.all._

/**
 * Enumerated type for wavelength band.
 * @group Enumerations
 */
sealed abstract class WavelengthBand(
  val tag:             String,
  val shortName:       String,
  val longName:        String,
  val center:          Wavelength,
  val width:           Quantity[PosInt, Nanometer],
  val BrightnessUnits: BrightnessUnits
) extends Product
    with Serializable {
  require(center.toPicometers.value >= width.value / 2)

  def end: Wavelength = Wavelength(
    center.toPicometers + (width.value.value / 2).withRefinedUnit[Positive, Nanometer]
  )

  // The require above lets ensure we can do this unsafe refine conversion
  import coulomb.refined.policy.unsoundRefinedConversions._
  def start: Wavelength = Wavelength(
    center.toPicometers - (width.value.value / 2).withRefinedUnit[Positive, Nanometer]
  )
}

object WavelengthBand {

  /** @group Constructors */
  case object SloanU
      extends WavelengthBand(
        "SloanU",
        "u",
        "UV",
        Wavelength(356000),
        46.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.ABMagnitudes
      )

  /** @group Constructors */
  case object SloanG
      extends WavelengthBand(
        "SloanG",
        "g",
        "Green",
        Wavelength(483000),
        99.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.ABMagnitudes
      )

  /** @group Constructors */
  case object SloanR
      extends WavelengthBand(
        "SloanR",
        "r",
        "Red",
        Wavelength(626000),
        96.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.ABMagnitudes
      )

  /** @group Constructors */
  case object SloanI
      extends WavelengthBand(
        "SloanI",
        "i",
        "Far red",
        Wavelength(767000),
        106.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.ABMagnitudes
      )

  /** @group Constructors */
  case object SloanZ
      extends WavelengthBand(
        "SloanZ",
        "z",
        "Near infrared",
        Wavelength(910000),
        125.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.ABMagnitudes
      )

  /** @group Constructors */
  case object U
      extends WavelengthBand(
        "U",
        "U",
        "Ultraviolet",
        Wavelength(360000),
        75.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object B
      extends WavelengthBand(
        "B",
        "B",
        "Blue",
        Wavelength(440000),
        90.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object V
      extends WavelengthBand(
        "V",
        "V",
        "Visual",
        Wavelength(550000),
        85.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object Uc
      extends WavelengthBand(
        "Uc",
        "UC",
        "UCAC",
        Wavelength(610000),
        63.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object R
      extends WavelengthBand(
        "R",
        "R",
        "Red",
        Wavelength(670000),
        100.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object I
      extends WavelengthBand(
        "I",
        "I",
        "Infrared",
        Wavelength(870000),
        100.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object Y
      extends WavelengthBand(
        "Y",
        "Y",
        "Y",
        Wavelength(1020000),
        120.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object J
      extends WavelengthBand(
        "J",
        "J",
        "J",
        Wavelength(1250000),
        240.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object H
      extends WavelengthBand(
        "H",
        "H",
        "H",
        Wavelength(1650000),
        300.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object K
      extends WavelengthBand(
        "K",
        "K",
        "K",
        Wavelength(2200000),
        410.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object L
      extends WavelengthBand(
        "L",
        "L",
        "L",
        Wavelength(3760000),
        700.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object M
      extends WavelengthBand(
        "M",
        "M",
        "M",
        Wavelength(4770000),
        240.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object N
      extends WavelengthBand(
        "N",
        "N",
        "N",
        Wavelength(10470000),
        5230.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object Q
      extends WavelengthBand(
        "Q",
        "Q",
        "Q",
        Wavelength(20130000),
        1650.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** @group Constructors */
  case object Ap
      extends WavelengthBand(
        "Ap",
        "AP",
        "Apparent",
        Wavelength(550000),
        85.withRefinedUnit[Positive, Nanometer],
        BrightnessUnits.VegaMagnitudes
      )

  /** All members of WavelengthBand, in canonical order. */
  val all: List[WavelengthBand] =
    List(SloanU, SloanG, SloanR, SloanI, SloanZ, U, B, V, Uc, R, I, Y, J, H, K, L, M, N, Q, Ap)

  /** Select the member of WavelengthBand with the given tag, if any. */
  def fromTag(s: String): Option[WavelengthBand] =
    all.find(_.tag === s)

  /** Select the member of WavelengthBand with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): WavelengthBand =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"WavelengthBand: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val WavelengthBandEnumerated: Enumerated[WavelengthBand] =
    new Enumerated[WavelengthBand] {
      def all                                               = WavelengthBand.all
      def tag(a: WavelengthBand)                            = a.tag
      override def unsafeFromTag(s: String): WavelengthBand =
        WavelengthBand.unsafeFromTag(s)
    }

  /** @group Typeclass Instances */
  val WavelengthBandWavelengthOrder: Order[WavelengthBand] =
    Order.by(_.center)

  /** @group Typeclass Instances */
  implicit val WavelengthBandOrdering: Ordering[WavelengthBand] =
    WavelengthBandEnumerated.toOrdering

}

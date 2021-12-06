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
import lucuma.core.math.BrightnessUnit
import lucuma.core.math.BrightnessValue
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

trait BandDefaultUnit {
  trait DefaultUnit[B <: Band, G <: BrightnessUnit.Group] {
    val unit: GroupedUnitType[G]

    def withValue(
      value: BrightnessValue
    ): GroupedUnitQuantity[BrightnessValue, BrightnessUnit.Group] =
      GroupedUnitQuantity(value, unit)
  }
  object DefaultUnit                                      {
    def apply[B <: Band, G <: BrightnessUnit.Group, U](implicit ev: UnitOfMeasure[U]) =
      new DefaultUnit[B, G] {
        val unit: GroupedUnitType[G] = ev.groupedIn[G]
      }
  }
}

trait BandDefaultUnitLowPriorityImplicits extends BandDefaultUnit {
  // These are the defaults
  implicit def defaultIntegratedUnit[B <: Band]: DefaultUnit[B, BrightnessUnit.Integrated] =
    DefaultUnit[B, BrightnessUnit.Integrated, VegaMagnitude]
  implicit def defaultSurfaceUnit[B <: Band]: DefaultUnit[B, BrightnessUnit.Surface]       =
    DefaultUnit[B, BrightnessUnit.Surface, VegaMagnitudePerArcsec2]
}

object Band extends BandDefaultUnitLowPriorityImplicits {

  /** @group Constructors */
  case object SloanU
      extends Band(
        "SloanU",
        "u",
        "UV",
        Wavelength(356000),
        46.withRefinedUnit[Positive, Nanometer]
      )
  implicit val defaultSloanUIntegratedUnit: DefaultUnit[SloanU.type, BrightnessUnit.Integrated] =
    DefaultUnit[SloanU.type, BrightnessUnit.Integrated, ABMagnitude]
  implicit val defaultSloanUSurfaceUnit: DefaultUnit[SloanU.type, BrightnessUnit.Surface]       =
    DefaultUnit[SloanU.type, BrightnessUnit.Surface, ABMagnitudePerArcsec2]

  /** @group Constructors */
  case object SloanG
      extends Band(
        "SloanG",
        "g",
        "Green",
        Wavelength(483000),
        99.withRefinedUnit[Positive, Nanometer]
      )
  implicit val defaultSloanGIntegratedUnit: DefaultUnit[SloanG.type, BrightnessUnit.Integrated] =
    DefaultUnit[SloanG.type, BrightnessUnit.Integrated, ABMagnitude]
  implicit val defaultSloanGSurfaceUnit: DefaultUnit[SloanG.type, BrightnessUnit.Surface]       =
    DefaultUnit[SloanG.type, BrightnessUnit.Surface, ABMagnitudePerArcsec2]

  /** @group Constructors */
  case object SloanR
      extends Band(
        "SloanR",
        "r",
        "Red",
        Wavelength(626000),
        96.withRefinedUnit[Positive, Nanometer]
      )
  implicit val defaultSloanRIntegratedUnit: DefaultUnit[SloanR.type, BrightnessUnit.Integrated] =
    DefaultUnit[SloanR.type, BrightnessUnit.Integrated, ABMagnitude]
  implicit val defaultSloanRSurfaceUnit: DefaultUnit[SloanR.type, BrightnessUnit.Surface]       =
    DefaultUnit[SloanR.type, BrightnessUnit.Surface, ABMagnitudePerArcsec2]

  /** @group Constructors */
  case object SloanI
      extends Band(
        "SloanI",
        "i",
        "Far red",
        Wavelength(767000),
        106.withRefinedUnit[Positive, Nanometer]
      )
  implicit val defaultSloanIIntegratedUnit: DefaultUnit[SloanI.type, BrightnessUnit.Integrated] =
    DefaultUnit[SloanI.type, BrightnessUnit.Integrated, ABMagnitude]
  implicit val defaultSloanISurfaceUnit: DefaultUnit[SloanI.type, BrightnessUnit.Surface]       =
    DefaultUnit[SloanI.type, BrightnessUnit.Surface, ABMagnitudePerArcsec2]

  /** @group Constructors */
  case object SloanZ
      extends Band(
        "SloanZ",
        "z",
        "Near infrared",
        Wavelength(910000),
        125.withRefinedUnit[Positive, Nanometer]
      )
  implicit val defaultSloanZIntegratedUnit: DefaultUnit[SloanZ.type, BrightnessUnit.Integrated] =
    DefaultUnit[SloanZ.type, BrightnessUnit.Integrated, ABMagnitude]
  implicit val defaultSloanZSurfaceUnit: DefaultUnit[SloanZ.type, BrightnessUnit.Surface]       =
    DefaultUnit[SloanZ.type, BrightnessUnit.Surface, ABMagnitudePerArcsec2]

  /** @group Constructors */
  case object U
      extends Band(
        "U",
        "U",
        "Ultraviolet",
        Wavelength(360000),
        75.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object B
      extends Band(
        "B",
        "B",
        "Blue",
        Wavelength(440000),
        90.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object V
      extends Band(
        "V",
        "V",
        "Visual",
        Wavelength(550000),
        85.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Uc
      extends Band(
        "Uc",
        "UC",
        "UCAC",
        Wavelength(610000),
        63.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object R
      extends Band(
        "R",
        "R",
        "Red",
        Wavelength(670000),
        100.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object I
      extends Band(
        "I",
        "I",
        "Infrared",
        Wavelength(870000),
        100.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Y
      extends Band(
        "Y",
        "Y",
        "Y",
        Wavelength(1020000),
        120.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object J
      extends Band(
        "J",
        "J",
        "J",
        Wavelength(1250000),
        240.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object H
      extends Band(
        "H",
        "H",
        "H",
        Wavelength(1650000),
        300.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object K
      extends Band(
        "K",
        "K",
        "K",
        Wavelength(2200000),
        410.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object L
      extends Band(
        "L",
        "L",
        "L",
        Wavelength(3760000),
        700.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object M
      extends Band(
        "M",
        "M",
        "M",
        Wavelength(4770000),
        240.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object N
      extends Band(
        "N",
        "N",
        "N",
        Wavelength(10470000),
        5230.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Q
      extends Band(
        "Q",
        "Q",
        "Q",
        Wavelength(20130000),
        1650.withRefinedUnit[Positive, Nanometer]
      )

  /** @group Constructors */
  case object Ap
      extends Band(
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

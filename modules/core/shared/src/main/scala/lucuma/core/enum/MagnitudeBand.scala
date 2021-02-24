// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum

import cats.Order
import cats.syntax.eq._
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated

/**
 * Enumerated type for magnitude band.
 * @group Enumerations (Generated)
 */
sealed abstract class MagnitudeBand(
  val tag: String,
  val shortName: String,
  val longName: String,
  val center: Wavelength,
  val width: Int,
  val magnitudeSystem: MagnitudeSystem
) extends Product with Serializable

object MagnitudeBand {

  /** @group Constructors */ case object SloanU extends MagnitudeBand("SloanU", "u", "UV", Wavelength.unsafeFromInt(356000), 46, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanG extends MagnitudeBand("SloanG", "g", "Green", Wavelength.unsafeFromInt(483000), 99, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanR extends MagnitudeBand("SloanR", "r", "Red", Wavelength.unsafeFromInt(626000), 96, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanI extends MagnitudeBand("SloanI", "i", "Far red", Wavelength.unsafeFromInt(767000), 106, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanZ extends MagnitudeBand("SloanZ", "z", "Near infrared", Wavelength.unsafeFromInt(910000), 125, MagnitudeSystem.AB)
  /** @group Constructors */ case object U extends MagnitudeBand("U", "U", "Ultraviolet", Wavelength.unsafeFromInt(360000), 75, MagnitudeSystem.Vega)
  /** @group Constructors */ case object B extends MagnitudeBand("B", "B", "Blue", Wavelength.unsafeFromInt(440000), 90, MagnitudeSystem.Vega)
  /** @group Constructors */ case object V extends MagnitudeBand("V", "V", "Visual", Wavelength.unsafeFromInt(550000), 85, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Uc extends MagnitudeBand("Uc", "UC", "UCAC", Wavelength.unsafeFromInt(610000), 63, MagnitudeSystem.Vega)
  /** @group Constructors */ case object R extends MagnitudeBand("R", "R", "Red", Wavelength.unsafeFromInt(670000), 100, MagnitudeSystem.Vega)
  /** @group Constructors */ case object I extends MagnitudeBand("I", "I", "Infrared", Wavelength.unsafeFromInt(870000), 100, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Y extends MagnitudeBand("Y", "Y", "Y", Wavelength.unsafeFromInt(1020000), 120, MagnitudeSystem.Vega)
  /** @group Constructors */ case object J extends MagnitudeBand("J", "J", "J", Wavelength.unsafeFromInt(1250000), 240, MagnitudeSystem.Vega)
  /** @group Constructors */ case object H extends MagnitudeBand("H", "H", "H", Wavelength.unsafeFromInt(1650000), 300, MagnitudeSystem.Vega)
  /** @group Constructors */ case object K extends MagnitudeBand("K", "K", "K", Wavelength.unsafeFromInt(2200000), 410, MagnitudeSystem.Vega)
  /** @group Constructors */ case object L extends MagnitudeBand("L", "L", "L", Wavelength.unsafeFromInt(3760000), 700, MagnitudeSystem.Vega)
  /** @group Constructors */ case object M extends MagnitudeBand("M", "M", "M", Wavelength.unsafeFromInt(4770000), 240, MagnitudeSystem.Vega)
  /** @group Constructors */ case object N extends MagnitudeBand("N", "N", "N", Wavelength.unsafeFromInt(10470000), 5230, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Q extends MagnitudeBand("Q", "Q", "Q", Wavelength.unsafeFromInt(20130000), 1650, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Ap extends MagnitudeBand("Ap", "AP", "Apparent", Wavelength.unsafeFromInt(550000), 85, MagnitudeSystem.Vega)

  /** All members of MagnitudeBand, in canonical order. */
  val all: List[MagnitudeBand] =
    List(SloanU, SloanG, SloanR, SloanI, SloanZ, U, B, V, Uc, R, I, Y, J, H, K, L, M, N, Q, Ap)

  /** Select the member of MagnitudeBand with the given tag, if any. */
  def fromTag(s: String): Option[MagnitudeBand] =
    all.find(_.tag === s)

  /** Select the member of MagnitudeBand with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): MagnitudeBand =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"MagnitudeBand: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val MagnitudeBandEnumerated: Enumerated[MagnitudeBand] =
    new Enumerated[MagnitudeBand] {
      def all = MagnitudeBand.all
      def tag(a: MagnitudeBand) = a.tag
      override def unsafeFromTag(s: String): MagnitudeBand =
        MagnitudeBand.unsafeFromTag(s)
    }

  /** @group Typeclass Instances */
  val MagnitudeBandWavelengthOrder: Order[MagnitudeBand] =
    Order.by(_.center)

  /** @group Typeclass Instances */
  implicit val MagnitudeBandOrdering: Ordering[MagnitudeBand] =
    MagnitudeBandEnumerated.toOrdering

}

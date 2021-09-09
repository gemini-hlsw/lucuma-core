// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum

import cats.syntax.eq._
import coulomb.Unitless
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.math.units

/**
 * Enumerated type for magnitude system.
 * @tparam A units
 * @group Enumerations (Generated)
 */
sealed abstract class MagnitudeSystem(
  val tag: String
) extends Product with Serializable {
  type Units
}

object MagnitudeSystem {

  /** @group Constructors */ case object Vega           extends MagnitudeSystem("Vega") {
                                                                  type Units = Unitless
                                                                }
  /** @group Constructors */ case object AB             extends MagnitudeSystem("AB") {
                                                                  type Units = Unitless
                                                                }
  /** @group Constructors */ case object Jy             extends MagnitudeSystem("Jy") {
                                                                  type Units = units.Jansky
                                                                }
  /** @group Constructors */ case object Watts          extends MagnitudeSystem("Watts") {
                                                                  type Units = units.WattsMag
                                                                }
  /** @group Constructors */ case object ErgsWavelength extends MagnitudeSystem("ErgsWavelength") {
                                                                  type Units = units.ErgsWavelengthMag
                                                                }
  /** @group Constructors */ case object ErgsFrequency  extends MagnitudeSystem("ErgsFrequency") {
                                                                  type Units = units.ErgsFrequencyMag
                                                                }
  /** All members of MagnitudeSystem, in canonical order. */
  val all: List[MagnitudeSystem] =
    List(Vega, AB, Jy, Watts, ErgsWavelength, ErgsFrequency)

  /** Select the member of MagnitudeSystem with the given tag, if any. */
  def fromTag(s: String): Option[MagnitudeSystem] =
    all.find(_.tag === s)

  /** Select the member of MagnitudeSystem with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): MagnitudeSystem =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"MagnitudeSystem: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val MagnitudeSystemEnumerated: Enumerated[MagnitudeSystem] =
    new Enumerated[MagnitudeSystem] {
      def all = MagnitudeSystem.all
      def tag(a: MagnitudeSystem) = a.tag
      override def unsafeFromTag(s: String): MagnitudeSystem =
        MagnitudeSystem.unsafeFromTag(s)
    }

  // This Display can be useful to nicely format the units
  val unitDisplay: Display[MagnitudeSystem] = Display.byShortName {
    case MagnitudeSystem.Watts          => "W/m²/µm"
    case MagnitudeSystem.ErgsWavelength => "erg/s/cm²/Å"
    case MagnitudeSystem.ErgsFrequency  => "erg/s/cm²/Hz"
    case a                              => a.tag
  }

}

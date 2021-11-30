// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum

import cats.syntax.eq._
import coulomb.Unitless
import lucuma.core.math.units
import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for brightness units.
 * @group Enumerations
 */
sealed abstract class BrightnessUnits(
  val tag:  String,
  val show: String
) extends Product
    with Serializable {
  type Units
}

object BrightnessUnits {

  /** @group Constructors */
  case object VegaMagnitudes extends BrightnessUnits("VegaMag", "Vega mag") {
    type Units = Unitless
  }

  /** @group Constructors */
  case object ABMagnitudes extends BrightnessUnits("ABMag", "AB mag") {
    type Units = Unitless
  }

  /** @group Constructors */
  case object Janskys extends BrightnessUnits("Jy", "Jy") {
    type Units = units.Jansky
  }

  /** @group Constructors */
  case object Watts extends BrightnessUnits("Watts", "W/m²/µm") {
    type Units = units.WattsMag
  }

  /** @group Constructors */
  case object ErgsWavelength extends BrightnessUnits("ErgsWavelength", "erg/s/cm²/Å") {
    type Units = units.ErgsWavelengthMag
  }

  /** @group Constructors */
  case object ErgsFrequency extends BrightnessUnits("ErgsFrequency", "erg/s/cm/Hz") {
    type Units = units.ErgsFrequencyMag
  }

  /** All members of BrightnessUnits, in canonical order. */
  val all: List[BrightnessUnits] =
    List(VegaMagnitudes, ABMagnitudes, Janskys, Watts, ErgsWavelength, ErgsFrequency)

  /** Select the member of BrightnessUnits with the given tag, if any. */
  def fromTag(s: String): Option[BrightnessUnits] =
    all.find(_.tag === s)

  /** Select the member of BrightnessUnits with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): BrightnessUnits =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"BrightnessUnits: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val BrightnessUnitsEnumerated: Enumerated[BrightnessUnits] =
    new Enumerated[BrightnessUnits] {
      def all                                                = BrightnessUnits.all
      def tag(a: BrightnessUnits)                            = a.tag
      override def unsafeFromTag(s: String): BrightnessUnits =
        BrightnessUnits.unsafeFromTag(s)
    }

  // This Display can be useful to nicely format the units
  val unitDisplay: Display[BrightnessUnits] = Display.byShortName(_.show)

}

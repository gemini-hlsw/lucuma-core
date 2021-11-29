// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import lucuma.core.enum.WavelengthBand
import lucuma.core.enum.BrightnessUnits
import lucuma.core.math.BrightnessValue
import monocle.Focus
import monocle.Lens

/**
 * Describes the brightness of a target on a given band
 */
final case class Brightness(
  value:  BrightnessValue,
  band:   WavelengthBand,
  error:  Option[BrightnessValue],
  system: BrightnessUnits
) {
  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${system.tag})"
  }
}

object Brightness {
  val value: Lens[Brightness, BrightnessValue] = Focus[Brightness](_.value)

  val band: Lens[Brightness, WavelengthBand] = Focus[Brightness](_.band)

  val error: Lens[Brightness, Option[BrightnessValue]] = Focus[Brightness](_.error)

  val system: Lens[Brightness, BrightnessUnits] = Focus[Brightness](_.system)

  /** Secondary constructor. */
  def apply(value: BrightnessValue, band: WavelengthBand, error: BrightnessValue) =
    new Brightness(value, band, Some(error), band.BrightnessUnits)

  /** Secondary constructor defaulting to no error. */
  def apply(value: BrightnessValue, band: WavelengthBand, system: BrightnessUnits) =
    new Brightness(value, band, None, system)

  /** Secondary constructor defaulting to no given error. */
  def apply(value: BrightnessValue, band: WavelengthBand) =
    new Brightness(value, band, None, band.BrightnessUnits)

  // by system name, band name, value and error (in that order)
  implicit val BrightnessOrdering: Order[Brightness] =
    Order.by(m => (m.system.tag, m.band.tag, m.value, m.error))

  /** group Typeclass Instances */
  implicit val BrightnessShow: Show[Brightness] =
    Show.fromToString

}

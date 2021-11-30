// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import lucuma.core.enum.Band
import lucuma.core.enum.BrightnessUnits
import lucuma.core.math.BrightnessValue
import monocle.Focus
import monocle.Lens

/**
 * Describes the brightness of a target on a given band.
 *
 * This class replaces the previous `Magnitude`.
 */
final case class Brightness(
  value: BrightnessValue,
  band:  Band,
  error: Option[BrightnessValue],
  units: BrightnessUnits
) {
  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${units.tag})"
  }
}

object Brightness {
  val value: Lens[Brightness, BrightnessValue] = Focus[Brightness](_.value)

  val band: Lens[Brightness, Band] = Focus[Brightness](_.band)

  val error: Lens[Brightness, Option[BrightnessValue]] = Focus[Brightness](_.error)

  val units: Lens[Brightness, BrightnessUnits] = Focus[Brightness](_.units)

  /** Secondary constructor. */
  def apply(value: BrightnessValue, band: Band, error: BrightnessValue) =
    new Brightness(value, band, Some(error), band.brightnessUnits)

  /** Secondary constructor defaulting to no error. */
  def apply(value: BrightnessValue, band: Band, units: BrightnessUnits) =
    new Brightness(value, band, None, units)

  /** Secondary constructor defaulting to no given error. */
  def apply(value: BrightnessValue, band: Band) =
    new Brightness(value, band, None, band.brightnessUnits)

  // by units name, band name, value and error (in that order)
  implicit val BrightnessOrdering: Order[Brightness] =
    Order.by(m => (m.units.tag, m.band.tag, m.value, m.error))

  /** group Typeclass Instances */
  implicit val BrightnessShow: Show[Brightness] =
    Show.fromToString

}

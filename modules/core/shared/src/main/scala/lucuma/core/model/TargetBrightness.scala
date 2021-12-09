// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional._
import monocle.Focus
import monocle.Lens

/**
 * Describes the brightness of a target on a given band.
 *
 * This class replaces the previous `Magnitude`.
 */
final case class TargetBrightness[B](
  quantity: GroupedUnitQuantity[BrightnessValue, B],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${quantity.value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${quantity.unit.definition.abbv})"
  }
}

object TargetBrightness {
  def quantity[B]: Lens[TargetBrightness[B], GroupedUnitQuantity[BrightnessValue, B]] =
    Focus[TargetBrightness[B]](_.quantity)

  def value[B]: Lens[TargetBrightness[B], BrightnessValue] =
    quantity.andThen(GroupedUnitQuantity.value)

  def unit[B]: Lens[TargetBrightness[B], GroupedUnitType[B]] =
    quantity.andThen(GroupedUnitQuantity.unit)

  def band[B]: Lens[TargetBrightness[B], Band] = Focus[TargetBrightness[B]](_.band)

  def error[B]: Lens[TargetBrightness[B], Option[BrightnessValue]] =
    Focus[TargetBrightness[B]](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[B](
    quantity: GroupedUnitQuantity[BrightnessValue, B],
    band:     Band
  ): TargetBrightness[B] =
    new TargetBrightness(quantity, band, none)

  /** Secondary constructor using default units for the band. */
  def apply[B] = new GroupApplied[B]
  protected class GroupApplied[B] {
    def apply[N <: Band](value: BrightnessValue, band: N, error: Option[BrightnessValue])(implicit
      ev:                       Band.DefaultUnit[N, B]
    ): TargetBrightness[B] =
      new TargetBrightness(ev.unit.withValue(value), band, error)

    def apply[N <: Band](value: BrightnessValue, band: N)(implicit
      ev:                       Band.DefaultUnit[N, B]
    ): TargetBrightness[B] =
      apply(value, band, none)
  }

  implicit def TargetBrightnessOrder[B](implicit
    unitOrder: Order[GroupedUnitType[B]]
  ): Order[TargetBrightness[B]] =
    Order.by(m => (m.quantity.unit, m.band.tag, m.quantity.value, m.error))

  /** group Typeclass Instances */
  implicit def TargetBrightnessShow[B]: Show[TargetBrightness[B]] =
    Show.fromToString

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional._
import monocle.Focus
import monocle.Lens

/**
 * Describes the brightness of a target on a given band.
 *
 * This class replaces the previous `Magnitude`.
 *
 * @tparam T
 *   The brightness unit type. For example: Integrated or Surface.
 */
final case class TargetBrightness[T](
  quantity: GroupedUnitQuantity[BrightnessValue, Brightness[T]],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${quantity.value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${quantity.unit.definition.abbv})"
  }
}

object TargetBrightness {
  def quantity[T]: Lens[TargetBrightness[T], GroupedUnitQuantity[BrightnessValue, Brightness[T]]] =
    Focus[TargetBrightness[T]](_.quantity)

  def value[T]: Lens[TargetBrightness[T], BrightnessValue] =
    quantity.andThen(GroupedUnitQuantity.value)

  def unit[T]: Lens[TargetBrightness[T], GroupedUnitType[Brightness[T]]] =
    quantity.andThen(GroupedUnitQuantity.unit)

  def band[T]: Lens[TargetBrightness[T], Band] = Focus[TargetBrightness[T]](_.band)

  def error[T]: Lens[TargetBrightness[T], Option[BrightnessValue]] =
    Focus[TargetBrightness[T]](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[T](
    quantity: GroupedUnitQuantity[BrightnessValue, Brightness[T]],
    band:     Band
  ): TargetBrightness[T] =
    new TargetBrightness(quantity, band, none)

  /** Secondary constructor using default units for the band. */
  def apply[T] = new GroupApplied[T]
  protected class GroupApplied[T] {
    def apply[B <: Band](value: BrightnessValue, band: B, error: Option[BrightnessValue])(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): TargetBrightness[T] =
      new TargetBrightness(ev.unit.withValue(value), band, error)

    def apply[B <: Band](value: BrightnessValue, band: B)(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): TargetBrightness[T] =
      apply(value, band, none)
  }

  implicit def TargetBrightnessOrder[T](implicit
    unitOrder: Order[GroupedUnitType[Brightness[T]]]
  ): Order[TargetBrightness[T]] =
    Order.by(m => (m.quantity.unit, m.band.tag, m.quantity.value, m.error))

  /** group Typeclass Instances */
  implicit def TargetBrightnessShow[UG]: Show[TargetBrightness[UG]] =
    Show.fromToString

}

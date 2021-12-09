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
 *
 * @tparam UG
 *   The unit group for the brightness. For example: Integrated or Surface.
 */
final case class TargetBrightness[UG](
  quantity: GroupedUnitQuantity[BrightnessValue, UG],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${quantity.value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${quantity.unit.definition.abbv})"
  }
}

object TargetBrightness {
  def quantity[UG]: Lens[TargetBrightness[UG], GroupedUnitQuantity[BrightnessValue, UG]] =
    Focus[TargetBrightness[UG]](_.quantity)

  def value[UG]: Lens[TargetBrightness[UG], BrightnessValue] =
    quantity.andThen(GroupedUnitQuantity.value)

  def unit[UG]: Lens[TargetBrightness[UG], GroupedUnitType[UG]] =
    quantity.andThen(GroupedUnitQuantity.unit)

  def band[UG]: Lens[TargetBrightness[UG], Band] = Focus[TargetBrightness[UG]](_.band)

  def error[UG]: Lens[TargetBrightness[UG], Option[BrightnessValue]] =
    Focus[TargetBrightness[UG]](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[UG](
    quantity: GroupedUnitQuantity[BrightnessValue, UG],
    band:     Band
  ): TargetBrightness[UG] =
    new TargetBrightness(quantity, band, none)

  /** Secondary constructor using default units for the band. */
  def apply[UG] = new GroupApplied[UG]
  protected class GroupApplied[UG] {
    def apply[B <: Band](value: BrightnessValue, band: B, error: Option[BrightnessValue])(implicit
      ev:                       Band.DefaultUnit[B, UG]
    ): TargetBrightness[UG] =
      new TargetBrightness(ev.unit.withValue(value), band, error)

    def apply[B <: Band](value: BrightnessValue, band: B)(implicit
      ev:                       Band.DefaultUnit[B, UG]
    ): TargetBrightness[UG] =
      apply(value, band, none)
  }

  implicit def TargetBrightnessOrder[UG](implicit
    unitOrder: Order[GroupedUnitType[UG]]
  ): Order[TargetBrightness[UG]] =
    Order.by(m => (m.quantity.unit, m.band.tag, m.quantity.value, m.error))

  /** group Typeclass Instances */
  implicit def TargetBrightnessShow[UG]: Show[TargetBrightness[UG]] =
    Show.fromToString

}

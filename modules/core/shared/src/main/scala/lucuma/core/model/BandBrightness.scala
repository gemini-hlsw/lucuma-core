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
final case class BandBrightness[T](
  quantity: GroupedUnitQty[BrightnessValue, Brightness[T]],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${quantity.value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${quantity.unit.definition.abbv})"
  }
}

object BandBrightness {
  def quantity[T]: Lens[BandBrightness[T], GroupedUnitQty[BrightnessValue, Brightness[T]]] =
    Focus[BandBrightness[T]](_.quantity)

  def value[T]: Lens[BandBrightness[T], BrightnessValue] =
    quantity.andThen(GroupedUnitQty.value)

  def unit[T]: Lens[BandBrightness[T], GroupedUnitType[Brightness[T]]] =
    quantity.andThen(GroupedUnitQty.unit)

  def band[T]: Lens[BandBrightness[T], Band] = Focus[BandBrightness[T]](_.band)

  def error[T]: Lens[BandBrightness[T], Option[BrightnessValue]] =
    Focus[BandBrightness[T]](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[T](
    quantity: GroupedUnitQty[BrightnessValue, Brightness[T]],
    band:     Band
  ): BandBrightness[T] =
    new BandBrightness(quantity, band, none)

  def apply[T](
    quantity: GroupedUnitQty[BrightnessValue, Brightness[T]],
    band:     Band,
    error:    BrightnessValue
  ): BandBrightness[T] =
    new BandBrightness(quantity, band, error.some)

  /** Secondary constructor using default units for the band. */
  def apply[T] = new GroupApplied[T]
  protected class GroupApplied[T] {
    def apply[B <: Band](value: BrightnessValue, band: B, error: Option[BrightnessValue])(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): BandBrightness[T] =
      new BandBrightness(ev.unit.withValue(value), band, error)

    def apply[B <: Band](value: BrightnessValue, band: B)(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): BandBrightness[T] =
      apply(value, band, none)

    def apply[B <: Band](value: BrightnessValue, band: B, error: BrightnessValue)(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): BandBrightness[T] = apply(value, band, error.some)
  }

  implicit def TargetBrightnessOrder[T](implicit
    unitOrder: Order[GroupedUnitType[Brightness[T]]]
  ): Order[BandBrightness[T]] =
    Order.by(m => (m.quantity.unit, m.band.tag, m.quantity.value, m.error))

  /** group Typeclass Instances */
  implicit def TargetBrightnessShow[UG]: Show[BandBrightness[UG]] =
    Show.fromToString

}

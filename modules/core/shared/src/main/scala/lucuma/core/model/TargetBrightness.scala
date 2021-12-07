// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnit
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional._
import monocle.Focus
import monocle.Lens

/**
 * Describes the brightness of a target on a given band.
 *
 * This class replaces the previous `Magnitude`.
 */
final case class TargetBrightness(
  quantity: GroupedUnitQuantity[BrightnessValue, BrightnessUnit.Group],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"Brightness(${quantity.value.toDoubleValue}%.2f, ${band.shortName}, $errStr, ${quantity.unit.definition.abbv})"
  }
}

object TargetBrightness {
  val quantity: Lens[TargetBrightness, GroupedUnitQuantity[BrightnessValue, BrightnessUnit.Group]] =
    Focus[TargetBrightness](_.quantity)

  val value: Lens[TargetBrightness, BrightnessValue] = quantity.andThen(GroupedUnitQuantity.value)

  val unit: Lens[TargetBrightness, GroupedUnitType[BrightnessUnit.Group]] =
    quantity.andThen(GroupedUnitQuantity.unit)

  val band: Lens[TargetBrightness, Band] = Focus[TargetBrightness](_.band)

  val error: Lens[TargetBrightness, Option[BrightnessValue]] = Focus[TargetBrightness](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[Units](
    quantity: GroupedUnitQuantity[BrightnessValue, BrightnessUnit.Group],
    band:     Band
  ): TargetBrightness =
    new TargetBrightness(quantity, band, none)

  /** Secondary constructor using default units for the band. */
  def apply[G <: BrightnessUnit.Group] = new GroupApplied[G]
  protected class GroupApplied[G <: BrightnessUnit.Group] {
    def apply[B <: Band](value: BrightnessValue, band: B, error: Option[BrightnessValue])(implicit
      ev:                       Band.DefaultUnit[B, G]
    ): TargetBrightness =
      new TargetBrightness(ev.unit.withValue(value), band, error)

    def apply[B <: Band](value: BrightnessValue, band: B)(implicit
      ev:                       Band.DefaultUnit[B, G]
    ): TargetBrightness =
      apply(value, band, none)
  }

  implicit def TargetBrightnessOrder: Order[TargetBrightness] =
    Order.by(m => (m.quantity.unit, m.band.tag, m.quantity.value, m.error))

  /** group Typeclass Instances */
  implicit def TargetBrightnessShow[Units]: Show[TargetBrightness] =
    Show.fromToString

}

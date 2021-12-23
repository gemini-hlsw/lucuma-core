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
import shapeless.tag.@@

/**
 * Describes the brightness of a target on a given band.
 *
 * This class replaces the previous `Magnitude`.
 *
 * @tparam T
 *   The brightness unit type. For example: Integrated or Surface.
 */
final case class BandBrightness[T](
  quantity: Qty[BrightnessValue] @@ Brightness[T],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"BandBrightness(${quantity.value.toDoubleValue}%.2f ${quantity.unit.abbv}, ${band.shortName}, $errStr)"
  }
}

object BandBrightness {

  /** @group Optics */
  def quantity[T]: Lens[BandBrightness[T], Qty[BrightnessValue] @@ Brightness[T]] =
    Focus[BandBrightness[T]](_.quantity)

  /** @group Optics */
  def value[T]: Lens[BandBrightness[T], BrightnessValue] =
    quantity.andThen(Qty.valueT)

  /** @group Optics */
  def unit[T]: Lens[BandBrightness[T], UnitType @@ Brightness[T]] =
    quantity.andThen(Qty.unitT)

  /** @group Optics */
  def band[T]: Lens[BandBrightness[T], Band] = Focus[BandBrightness[T]](_.band)

  /** @group Optics */
  def error[T]: Lens[BandBrightness[T], Option[BrightnessValue]] =
    Focus[BandBrightness[T]](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[T](
    quantity: Qty[BrightnessValue] @@ Brightness[T],
    band:     Band
  ): BandBrightness[T] =
    new BandBrightness(quantity, band, none)

  /** Secondary constructor with error. */
  def apply[T](
    quantity: Qty[BrightnessValue] @@ Brightness[T],
    band:     Band,
    error:    BrightnessValue
  ): BandBrightness[T] =
    new BandBrightness(quantity, band, error.some)

  /** Secondary constructor using type units. */
  def apply[T, U](value: BrightnessValue, band: Band, error: Option[BrightnessValue])(implicit
    tagged:              IsTaggedUnit[U, Brightness[T]]
  ): BandBrightness[T] =
    new BandBrightness(tagged.unit.withValueT(value), band, error)

  /** Secondary constructor using type units and no error. */
  def apply[T, U](value: BrightnessValue, band: Band)(implicit
    tagged:              IsTaggedUnit[U, Brightness[T]]
  ): BandBrightness[T] =
    apply[T, U](value, band, none)

  /** Secondary constructor with error and using type units. */
  def apply[T, U](value: BrightnessValue, band: Band, error: BrightnessValue)(implicit
    tagged:              IsTaggedUnit[U, Brightness[T]]
  ): BandBrightness[T] =
    apply[T, U](value, band, error.some)

  /** Secondary constructor using default units for the band. */
  def apply[T] = new GroupApplied[T]
  protected class GroupApplied[T] {

    /** Secondary constructor using default units for the band. */
    def apply[B <: Band](value: BrightnessValue, band: B, error: Option[BrightnessValue])(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): BandBrightness[T] =
      new BandBrightness(ev.unit.withValueT(value), band, error)

    /** Secondary constructor using default units for the band and no error. */
    def apply[B <: Band](value: BrightnessValue, band: B)(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): BandBrightness[T] =
      apply(value, band, none)

    /** Secondary constructor with error and using default units for the band. */
    def apply[B <: Band](value: BrightnessValue, band: B, error: BrightnessValue)(implicit
      ev:                       Band.DefaultUnit[B, T]
    ): BandBrightness[T] = apply(value, band, error.some)
  }

  /** group Typeclass Instances */
  implicit def bandBrightnessOrder[T](implicit
    unitOrder: Order[UnitType @@ Brightness[T]]
  ): Order[BandBrightness[T]] =
    Order.by(m => (Qty.unitT.get(m.quantity), m.band.tag, Qty.value.get(m.quantity), m.error))

  /** group Typeclass Instances */
  implicit def bandBrightnessShow[T]: Show[BandBrightness[T]] =
    Show.fromToString

}

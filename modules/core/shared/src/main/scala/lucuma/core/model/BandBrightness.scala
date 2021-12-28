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
  quantity: Measure[BrightnessValue] Of Brightness[T],
  band:     Band,
  error:    Option[BrightnessValue]
) {

  /**
   * Convert units to `T0` brightness type.
   *
   * @tparam `T0`
   *   `Integrated` or `Surface`
   */
  def to[T0](implicit
    conv: TagConverter[Brightness[T], Brightness[T0]]
  ): BandBrightness[T0] = BandBrightness[T0](quantity.toTag[Brightness[T0]], band, error)

  override def toString: String = {
    val errStr = error.map(e => f"${e.toDoubleValue}%.2f")
    f"BandBrightness(${quantity.value.toDoubleValue}%.2f ${quantity.units.abbv}, ${band.shortName}, $errStr)"
  }
}

object BandBrightness {

  /** @group Optics */
  def quantity[T]: Lens[BandBrightness[T], Measure[BrightnessValue] Of Brightness[T]] =
    Focus[BandBrightness[T]](_.quantity)

  /** @group Optics */
  def value[T]: Lens[BandBrightness[T], BrightnessValue] =
    quantity.andThen(Measure.valueTagged)

  /** @group Optics */
  def units[T]: Lens[BandBrightness[T], Units Of Brightness[T]] =
    quantity.andThen(Measure.unitsTagged)

  /** @group Optics */
  def band[T]: Lens[BandBrightness[T], Band] = Focus[BandBrightness[T]](_.band)

  /** @group Optics */
  def error[T]: Lens[BandBrightness[T], Option[BrightnessValue]] =
    Focus[BandBrightness[T]](_.error)

  /** Secondary constructor defaulting to no error. */
  def apply[T](
    quantity: Measure[BrightnessValue] Of Brightness[T],
    band:     Band
  ): BandBrightness[T] =
    new BandBrightness(quantity, band, none)

  /** Secondary constructor with error. */
  def apply[T](
    quantity: Measure[BrightnessValue] Of Brightness[T],
    band:     Band,
    error:    BrightnessValue
  ): BandBrightness[T] =
    new BandBrightness(quantity, band, error.some)

  /** Secondary constructor using type units. */
  def apply[T, U](value: BrightnessValue, band: Band, error: Option[BrightnessValue])(implicit
    tagged:              TaggedUnit[U, Brightness[T]]
  ): BandBrightness[T] =
    new BandBrightness(tagged.unit.withValueTagged(value), band, error)

  /** Secondary constructor using type units and no error. */
  def apply[T, U](value: BrightnessValue, band: Band)(implicit
    tagged:              TaggedUnit[U, Brightness[T]]
  ): BandBrightness[T] =
    apply[T, U](value, band, none)

  /** Secondary constructor with error and using type units. */
  def apply[T, U](value: BrightnessValue, band: Band, error: BrightnessValue)(implicit
    tagged:              TaggedUnit[U, Brightness[T]]
  ): BandBrightness[T] =
    apply[T, U](value, band, error.some)

  /** Secondary constructor using default units for the band. */
  def apply[T] = new GroupApplied[T]
  protected class GroupApplied[T] {

    /** Secondary constructor using default units for the band. */
    def apply(
      value: BrightnessValue,
      band:  Band,
      error: Option[BrightnessValue]
    )(implicit
      ev:    Band.DefaultUnit[band.type, T]
    ): BandBrightness[T] =
      new BandBrightness(band.defaultUnit[T].withValueTagged(value), band, error)

    /** Secondary constructor using default units for the band and no error. */
    def apply(value: BrightnessValue, band: Band)(implicit
      ev:            Band.DefaultUnit[band.type, T]
    ): BandBrightness[T] =
      apply(value, band, none)

    /** Secondary constructor with error and using default units for the band. */
    def apply(value: BrightnessValue, band: Band, error: BrightnessValue)(implicit
      ev:            Band.DefaultUnit[band.type, T]
    ): BandBrightness[T] = apply(value, band, error.some)
  }

  /** group Typeclass Instances */
  implicit def bandBrightnessOrder[T](implicit
    unitsOrder: Order[Units Of Brightness[T]]
  ): Order[BandBrightness[T]] =
    Order.by(m =>
      (Measure.unitsTagged.get(m.quantity), m.band.tag, Measure.value.get(m.quantity), m.error)
    )

  /** group Typeclass Instances */
  implicit def bandBrightnessShow[T]: Show[BandBrightness[T]] =
    Show.fromToString

}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import coulomb.Quantity
import coulomb.syntax.*
import lucuma.core.math.units.Angstrom
import lucuma.core.math.units.Micrometer
import lucuma.core.math.units.Nanometer
import lucuma.core.math.units.Picometer
import lucuma.core.optics.Format
import monocle.Iso

import java.math.RoundingMode


/**
 * A wavelength dither is a small offset (positive or negative) intended to be
 * applied to a wavelength, such as the observing wavelength.  It is therefore a
 * small distance and not a `Wavelength` per se.  A `Wavelength + WavelengthDither`
 * should produce a new `Wavelength`.
 */
opaque type WavelengthDither = Quantity[Int, Picometer]

object WavelengthDither {

  val Zero: WavelengthDither =
    Quantity[Picometer](0)

  extension (w: WavelengthDither) {
    def toPicometers: Quantity[Int, Picometer] =
      w

    private def to[U](scale: Int): Quantity[BigDecimal, U] =
      Quantity[U](BigDecimal(w.value, scale))

    def toAngstroms: Quantity[BigDecimal, Angstrom] =
      to[Angstrom](2)

    /**
     * Alias for `toAngstroms`.
     */
    def Å: Quantity[BigDecimal, Angstrom] =
      toAngstroms

    def toNanometers: Quantity[BigDecimal, Nanometer] =
      to[Nanometer](3)

   /**
     * Alias for `toNanometers`.
     */
    def nm: Quantity[BigDecimal, Nanometer] =
      toNanometers

    def toMicrometers: Quantity[BigDecimal, Micrometer] =
      to[Micrometer](scale = 6)

    def abs: WavelengthDither =
      w.value.abs.withUnit[Picometer]

    /** Alias for `toMicrometers`. */
    def µm: Quantity[BigDecimal, Micrometer] =
      toMicrometers
  }

  def apply(pm: Quantity[Int, Picometer]): WavelengthDither =
    pm

  private def fromBigDecimal[U](right: Int)(v: Quantity[BigDecimal, U]): Option[WavelengthDither] =
    (scala.util.control.Exception.catching(classOf[ArithmeticException]) opt
      v.value
       .bigDecimal
       .movePointRight(right)
       .setScale(0, RoundingMode.HALF_UP)
       .intValueExact
    ).map(Quantity[Picometer](_))

  val picometers: Iso[Quantity[Int, Picometer], WavelengthDither] =
    Iso[Quantity[Int, Picometer], WavelengthDither](apply)(_.toPicometers)

  val angstroms: Format[Quantity[BigDecimal, Angstrom], WavelengthDither] =
    Format(fromBigDecimal(2), _.toAngstroms)

  val nanometers: Format[Quantity[BigDecimal, Nanometer], WavelengthDither] =
    Format(fromBigDecimal(3), _.toNanometers)

  val micrometers: Format[Quantity[BigDecimal, Micrometer], WavelengthDither] =
    Format(fromBigDecimal(6), _.toMicrometers)

  val intPicometers: Iso[Int, WavelengthDither] =
    Iso[Int, Quantity[Int, Picometer]](Quantity[Picometer](_))(_.value).andThen(picometers)

  private def scalingFormat(move: Int): Format[BigDecimal, WavelengthDither] =
    Format[BigDecimal, Int](
      bd => scala.util.control.Exception.catching(classOf[ArithmeticException]) opt
        bd.bigDecimal.movePointRight(move).setScale(0, RoundingMode.HALF_UP).intValueExact,
      i  => BigDecimal(new java.math.BigDecimal(i).movePointLeft(move))
    ).andThen(intPicometers)

  val decimalPicometers: Format[BigDecimal, WavelengthDither] =
    scalingFormat(0)

  val decimalAngstroms: Format[BigDecimal, WavelengthDither] =
    scalingFormat(2)

  val decimalNanometers: Format[BigDecimal, WavelengthDither] =
    scalingFormat(3)

  val decimalMicrometers: Format[BigDecimal, WavelengthDither] =
    scalingFormat(6)

  given Order[WavelengthDither] =
    Order.by(_.toPicometers.value)

}

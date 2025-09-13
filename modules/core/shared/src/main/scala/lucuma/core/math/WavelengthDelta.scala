// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import algebra.instances.all.given
import cats.Order
import cats.Show
import coulomb.Quantity
import coulomb.syntax.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units.Angstrom
import lucuma.core.math.units.Micrometer
import lucuma.core.math.units.Nanometer
import lucuma.core.math.units.Picometer
import lucuma.core.optics.Format
import monocle.Iso
import monocle.Prism

import java.math.RoundingMode
import scala.annotation.targetName

/**
 * Represents a span of the spectrum, represented as a small positive wavelength
 * delta in picometers. It can be "anchored" by providing a starting, ending, or
 * central `Wavelength`, which results in a wavelength interval (ie: a
 * `BoundedInterval[Wavelength]`).
 */
opaque type WavelengthDelta = Quantity[PosInt, Picometer]

object WavelengthDelta:
  extension (w: WavelengthDelta) {
    def toPicometers: Quantity[PosInt, Picometer] =
      w

    /**
     * Alias for `toPicometers`.
     */
    def pm: Quantity[PosInt, Picometer] =
      toPicometers

    /**
      * Create a wavelength range centered on the given value. Will be truncated at
      * Wavelength.Min and Wavelength.Max.
      */
    def centeredAt(λ: Wavelength): BoundedInterval[Wavelength] =
      val start =  λ.pm.value.value - w.value.value / 2
      BoundedInterval.unsafeClosed(
        Wavelength(PosInt.unsafeFrom(math.max(Wavelength.Min.pm.value.value, start))),
        Wavelength(PosInt.unsafeFrom(math.min(Wavelength.Max.pm.value.value - pm.value.value, start) + pm.value.value))
      )

    /**
      * Create a wavelength range starting on the given value.  Will be truncated at Wavelength.Max.
      */
    def startingAt(λ: Wavelength): BoundedInterval[Wavelength] =
      BoundedInterval.unsafeClosed(λ, Wavelength(PosInt.unsafeFrom(
        math.min(Wavelength.Max.pm.value.value - pm.value.value, λ.pm.value.value) + pm.value.value
      )))

    /**
      * Create a wavelength range ending on the given value. Will be truncated at Wavelength.Min.
      */
    def endingAt(λ: Wavelength): BoundedInterval[Wavelength] =
      BoundedInterval.unsafeClosed(
        Wavelength(PosInt.unsafeFrom(math.max(Wavelength.Min.pm.value.value, λ.pm.value.value - w.value.value))),
        λ
      )

    // Conversion between units is guaranteed to be positive since the Wavelength in pm is positive.
    // The value can always be exactly represented as a (Pos)BigDecimal since sub-pm fractions cannot be
    // represented.
    private def to[U](scale: Int): Quantity[PosBigDecimal, U] =
      PosBigDecimal.unsafeFrom(BigDecimal(toPicometers.value.value, scale)).withUnit[U]

    /**
     * Returns the wavelength value in angstroms.
     */
    def toAngstroms: Quantity[PosBigDecimal, Angstrom] =
      to[Angstrom](2)

    /**
     * Alias for `toAngstroms`.
     */
    def Å: Quantity[PosBigDecimal, Angstrom] =
      toAngstroms

    /**
     * Returns the wavelength value in nanometers.
     */
    def toNanometers: Quantity[PosBigDecimal, Nanometer] =
      to[Nanometer](3)

    /**
     * Alias for `toNanometers`.
     */
    def nm: Quantity[PosBigDecimal, Nanometer] =
      toNanometers

    /**
     * Returns the wavelength value in microns.
     */
    def toMicrometers: Quantity[PosBigDecimal, Micrometer] =
      to[Micrometer](6)

    /** Alias for `toMicrometers`. */
    def µm: Quantity[PosBigDecimal, Micrometer] =
      toMicrometers
  }

  def apply(pm: Quantity[PosInt, Picometer]): WavelengthDelta =
    pm

  /**
    * Construct a wavelength range from a positive int
    * @group constructor
    */
  @targetName("applyPicometers") // to distinguish from apply(Quantity[PosInt, Picometer])
  def apply(picometers: PosInt): WavelengthDelta =
    picometers.withUnit[Picometer]

  /** @group Typeclass Instances */
  given Show[WavelengthDelta] =
    Show.fromToString

  /** @group Typeclass Instances */
  given Order[WavelengthDelta] =
    Order.by(_.value)

  val picometers: Iso[PosInt, WavelengthDelta] =
    Iso[PosInt, WavelengthDelta](_.withUnit[Picometer])(_.toPicometers.value)

  def pbdFormat[U](right: Int)(to: WavelengthDelta => Quantity[PosBigDecimal, U]): Format[Quantity[PosBigDecimal, U], WavelengthDelta] = {
    def from(v: Quantity[PosBigDecimal, U]): Option[WavelengthDelta] =
      (scala.util.control.Exception.catching(classOf[ArithmeticException]) opt
        v.value
         .bigDecimal
         .movePointRight(right)
         .setScale(0, RoundingMode.HALF_UP)
         .intValueExact
      ).flatMap(i => PosInt.from(i).toOption).map(_.withUnit[Picometer])

    Format[Quantity[PosBigDecimal, U], WavelengthDelta](from, to)
  }

  val angstroms: Format[Quantity[PosBigDecimal, Angstrom], WavelengthDelta] =
    pbdFormat(2)(_.toAngstroms)

  val nanometers: Format[Quantity[PosBigDecimal, Nanometer], WavelengthDelta] =
    pbdFormat(3)(_.toNanometers)

  val micrometers: Format[Quantity[PosBigDecimal, Micrometer], WavelengthDelta] =
    pbdFormat(6)(_.toMicrometers)

  val intPicometers: Prism[Int, WavelengthDelta] =
    Prism[Int, WavelengthDelta](pm =>
      PosInt.from(pm).toOption.map(_.withUnit[Picometer])
    )(_.value.value)

  /**
   * Try to build a Wavelength from a plain Int. Negatives and Zero will produce a None.
   * @group constructor
   */
  def fromIntPicometers(i: Int): Option[WavelengthDelta] =
    intPicometers.getOption(i)

  def unsafeFromIntPicometers(i: Int): WavelengthDelta =
    fromIntPicometers(i).getOrElse(sys.error(s"Cannot build a WavelengthDelta with value $i"))

  private def fromInt(max: Int, mult: Int): Int => Option[WavelengthDelta] =
    i => Option.when((i > 0) && (i <= max))(i * mult).flatMap(fromIntPicometers)

  def fromIntAngstroms(i: Int): Option[WavelengthDelta] =
    fromInt(Wavelength.MaxAngstrom, 100)(i)

  def fromIntNanometers(i: Int): Option[WavelengthDelta] =
    fromInt(Wavelength.MaxNanometer, 1_000)(i)

  def fromIntMicrometers(i: Int): Option[WavelengthDelta] =
    fromInt(Wavelength.MaxMicrometer, 1_000_000)(i)

  private def scalingFormat(move: Int): Format[BigDecimal, WavelengthDelta] =
    Format[BigDecimal, Int](
      bd => scala.util.control.Exception.catching(classOf[ArithmeticException]) opt
        bd.bigDecimal.movePointRight(move).setScale(0, RoundingMode.HALF_UP).intValueExact,
      i  => BigDecimal(new java.math.BigDecimal(i).movePointLeft(move))
    ).andThen(intPicometers)

  val decimalPicometers: Format[BigDecimal, WavelengthDelta] =
    scalingFormat(0)

  val decimalAngstroms: Format[BigDecimal, WavelengthDelta] =
    scalingFormat(2)

  val decimalNanometers: Format[BigDecimal, WavelengthDelta] =
    scalingFormat(3)

  val decimalMicrometers: Format[BigDecimal, WavelengthDelta] =
    scalingFormat(6)

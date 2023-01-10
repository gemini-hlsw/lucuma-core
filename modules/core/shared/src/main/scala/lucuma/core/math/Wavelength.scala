// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import cats.syntax.option.*
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units.{_, given}
import lucuma.core.optics.Format
import monocle.Iso
import monocle.Prism

import java.math.RoundingMode
import scala.annotation.targetName
import scala.util.Try

/**
  * Exact wavelengths represented as positive integral picometers in the range (0 .. PosInt.MaxValue]
  * which means the largest representable wavelength is 2.147483647 mm.
  * @param toPicometers This wavelength in positive integral picometers (10^-12^ of a meter).
  */
opaque type Wavelength = Quantity[PosInt, Picometer]

object Wavelength {

  lazy val Min: Wavelength   = unsafeFromIntPicometers(1)
  lazy val Max: Wavelength   = unsafeFromIntPicometers(Int.MaxValue)

  // Max allowed value in angstrom
  lazy val MaxAngstrom: PosInt   = PosInt.unsafeFrom(Int.MaxValue / 100)

  // Max allowed value in nanometers
  lazy val MaxNanometer: PosInt  = PosInt.unsafeFrom(Int.MaxValue / 1_000)

  // Max allowed value in microns
  lazy val MaxMicrometer: PosInt = PosInt.unsafeFrom(Int.MaxValue / 1_000_000)

  extension (w: Wavelength) {

    def toPicometers: Quantity[PosInt, Picometer] =
      w

    /**
     * Alias for `toPicometers`.
     */
    def pm: Quantity[PosInt, Picometer] =
      toPicometers

    // Conversion between units is guaranteed to be positive since the Wavelength in pm is positive.
    // The value can always be exactly represented as a (Pos)BigDecimal since sub-pm fractions cannot be
    // represented.
    private def to[U](scale: Int): Quantity[PosBigDecimal, U] =
      Quantity[U](PosBigDecimal.unsafeFrom(BigDecimal(toPicometers.value.value, scale)))

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

    /**
     * Adds the given offset to this wavelength.
     */
    def offset(wd: WavelengthDither): Option[Wavelength] =
      intPicometers.getOption(
        w.toPicometers.value.value + wd.toPicometers.value
      )

    def unsafeOffset(wd: WavelengthDither): Wavelength =
      offset(wd).get
  }

  def apply(pm: Quantity[PosInt, Picometer]): Wavelength =
    pm

  /**
    * Construct a wavelength from a positive int
    * @group constructor
    */
  @targetName("applyPicometers") // to distinguish from apply(Quantity[PosInt, Picometer])
  def apply(picometers: PosInt): Wavelength =
    picometers.withUnit[Picometer]

  /** @group Typeclass Instances */
  given Show[Wavelength] =
    Show.fromToString

  /** @group Typeclass Instances */
  given Order[Wavelength] =
    Order.by(_.value)

  val picometers: Iso[PosInt, Wavelength] =
    Iso[PosInt, Wavelength](_.withUnit[Picometer])(_.toPicometers.value)

  def pbdFormat[U](right: Int)(to: Wavelength => Quantity[PosBigDecimal, U]): Format[Quantity[PosBigDecimal, U], Wavelength] = {
    def from(v: Quantity[PosBigDecimal, U]): Option[Wavelength] =
      (scala.util.control.Exception.catching(classOf[ArithmeticException]) opt
        v.value
         .bigDecimal
         .movePointRight(right)
         .setScale(0, RoundingMode.HALF_UP)
         .intValueExact
      ).flatMap(i => PosInt.from(i).toOption).map(Quantity[Picometer](_))

    Format[Quantity[PosBigDecimal, U], Wavelength](from, to)
  }

  val angstroms: Format[Quantity[PosBigDecimal, Angstrom], Wavelength] =
    pbdFormat(2)(_.toAngstroms)

  val nanometers: Format[Quantity[PosBigDecimal, Nanometer], Wavelength] =
    pbdFormat(3)(_.toNanometers)

  val micrometers: Format[Quantity[PosBigDecimal, Micrometer], Wavelength] =
    pbdFormat(6)(_.toMicrometers)

  val intPicometers: Prism[Int, Wavelength] =
    Prism[Int, Wavelength](pm =>
      refineV[Positive](pm).toOption.map(_.withUnit[Picometer])
    )(_.value.value)

  /**
   * Try to build a Wavelength from a plain Int. Negatives and Zero will produce a None.
   * @group constructor
   */
  def fromIntPicometers(i: Int): Option[Wavelength] =
    intPicometers.getOption(i)

  def unsafeFromIntPicometers(i: Int): Wavelength =
    fromIntPicometers(i).getOrElse(sys.error(s"Cannot build a Wavelength with value $i"))

  private def fromInt(max: Int, mult: Int): Int => Option[Wavelength] =
    i => Option.when((i > 0) && (i <= max))(i * mult).flatMap(fromIntPicometers)

  def fromIntAngstroms(i: Int): Option[Wavelength] =
    fromInt(MaxAngstrom, 100)(i)

  def fromIntNanometers(i: Int): Option[Wavelength] =
    fromInt(MaxNanometer, 1_000)(i)

  def fromIntMicrometers(i: Int): Option[Wavelength] =
    fromInt(MaxMicrometer, 1_000_000)(i)

  private def scalingFormat(move: Int): Format[BigDecimal, Wavelength] =
    Format[BigDecimal, Int](
      bd => scala.util.control.Exception.catching(classOf[ArithmeticException]) opt
        bd.bigDecimal.movePointRight(move).setScale(0, RoundingMode.HALF_UP).intValueExact,
      i  => BigDecimal(new java.math.BigDecimal(i).movePointLeft(move))
    ).andThen(intPicometers)

  val decimalPicometers: Format[BigDecimal, Wavelength] =
    scalingFormat(0)

  val decimalAngstroms: Format[BigDecimal, Wavelength] =
    scalingFormat(2)

  val decimalNanometers: Format[BigDecimal, Wavelength] =
    scalingFormat(3)

  val decimalMicrometers: Format[BigDecimal, Wavelength] =
    scalingFormat(6)

}

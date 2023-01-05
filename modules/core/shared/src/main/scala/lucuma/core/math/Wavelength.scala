// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
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
final case class Wavelength(toPicometers: Quantity[PosInt, Picometer]) {

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
   * Returns the wavelength value in microns.
   */
  def toMicrometers: Quantity[PosBigDecimal, Micrometer] =
    to[Micrometer](6)

  /** Alias for `toMicrometers`. */
  def µm: Quantity[PosBigDecimal, Micrometer] =
    toMicrometers

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
   * Returns the wavelength value in angstroms.
   */
  def toAngstroms: Quantity[PosBigDecimal, Angstrom] =
    to[Angstrom](2)

  /**
   * Alias for `toAngstroms`.
   */
  def Å: Quantity[PosBigDecimal, Angstrom] =
    toAngstroms

  override def toString: String =
    s"Wavelength(${toPicometers.show})"

}

object Wavelength {
  lazy val Min: Wavelength   = unsafeFromInt(1)
  lazy val Max: Wavelength   = unsafeFromInt(Int.MaxValue)

  // Max allowed value in microns
  lazy val MaxMicrometer: Int = Int.MaxValue / BigInt(10).pow(6).toInt
  // Max allowed value in nanometers
  lazy val MaxNanometer: Int = Int.MaxValue / BigInt(10).pow(3).toInt
  // Max allowed value in angstrom
  lazy val MaxAngstrom: Int  = Int.MaxValue / BigInt(10).pow(2).toInt

  /**
    * Construct a wavelength from a positive int
    * @group constructor
    */
  @targetName("applyPicometers") // to distinguish from apply(Quantity[PosInt, Picometer])
  def apply(picometers: PosInt): Wavelength =
    new Wavelength(picometers.withUnit[Picometer])

  /** @group Typeclass Instances */
  given Show[Wavelength] =
    Show.fromToString

  /** @group Typeclass Instances */
  given Order[Wavelength] =
    Order.by(_.toPicometers)

  /**
    * Try to build a Wavelength from a plain Int. Negatives and Zero will produce a None
    * @group constructor
    */
  def fromInt(i: Int): Option[Wavelength] =
    refineV[Positive](i).toOption.map(apply)

  def unsafeFromInt(i: Int): Wavelength =
    fromInt(i).getOrElse(sys.error(s"Cannot build a Wavelength with value $i"))

  /**
   * Try to build a Wavelength with a value in nm in the range (0 .. 2147]
   * @group constructor
   */
  def fromMicrometers(µm: Int): Option[Wavelength] = for {
    q <- refineQV[Positive](µm.withUnit[Micrometer].tToUnit[Picometer]).toOption
    if µm <= MaxMicrometer
  } yield Wavelength(q)

  /**
   * Try to build a Wavelength with a value in nm in the range (0 .. 2147483]
   * @group constructor
   */
  def fromNanometers(nm: Int): Option[Wavelength] = for {
    q <- refineQV[Positive](nm.withUnit[Nanometer].tToUnit[Picometer]).toOption
    if nm <= MaxNanometer
  } yield Wavelength(q)

  /**
   * Try to build a Wavelength with a value in angstrom in the range (0 .. 21474836]
   * @group constructor
   */
  def fromAngstroms(a: Int): Option[Wavelength] = for {
    q <- refineQV[Positive](a.withUnit[Angstrom].tToUnit[Picometer]).toOption
    if a <= MaxAngstrom
  } yield Wavelength(q)

  /**
   * Prism from Int in pm into Wavelength and back.
   * @group Optics
   */
  val fromPicometers: Prism[Int, Wavelength] =
    Prism[Int, Wavelength](pm =>
      refineV[Positive](pm).toOption.map(v => Wavelength(v.withUnit[Picometer]))
    )(_.toPicometers.value)

  /**
   * Iso from PosInt in pm into Wavelength and back.
   * @group Optics
   */
  val picometers: Iso[PosInt, Wavelength] =
    Iso[PosInt, Wavelength](i => Wavelength(i.withUnit[Picometer]))(_.toPicometers.value)

  private def scalingFormat(move: Int): Format[BigDecimal, Wavelength] =
    Format[BigDecimal, Int](
      bd => Try(bd.underlying.movePointRight(move).setScale(0, RoundingMode.HALF_UP).intValueExact()).toOption,
      i  => BigDecimal(new java.math.BigDecimal(i).movePointLeft(move))
    ).andThen(fromPicometers)

  val decimalPicometers: Format[BigDecimal, Wavelength] =
    scalingFormat(0)

  val decimalAngstroms: Format[BigDecimal, Wavelength] =
    scalingFormat(2)

  val decimalNanometers: Format[BigDecimal, Wavelength] =
    scalingFormat(3)

  val decimalMicrometers: Format[BigDecimal, Wavelength] =
    scalingFormat(6)

}

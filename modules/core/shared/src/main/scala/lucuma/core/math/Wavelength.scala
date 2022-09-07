// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.Show
import coulomb.*
import coulomb.ops.algebra.cats.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units.{_, given}
import lucuma.core.optics.Format
import monocle.Iso
import monocle.Prism
import spire.math.Rational

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
   * Returns the wavelength value in microns. The exact microns value must be
   * represented as a Rational.
   */
  def µm: Quantity[Rational, Micrometer] =
    toPicometers.toValue[Rational].toUnit[Micrometer]

  /** Alias for `µm`. */
  def micrometer: Quantity[Rational, Micrometer] =
    µm

  /** Alias for `µm`. */
  def micron: Quantity[Rational, Micrometer] =
    µm

  /**
    * Returns the wavelength value in nanometers
    * The exact nanometer value needs to be represented as a Rational
    */
  def nm: Quantity[Rational, Nanometer] = toPicometers.toValue[Rational].toUnit[Nanometer]

  def nanometer: Quantity[Rational, Nanometer] = nm

  /**
    * Returns the wavelength value in angstrom
    * The exact angstrom value needs to be represented as a Rational
    */
  def Å: Quantity[Rational, Angstrom] = toPicometers.toValue[Rational].toUnit[Angstrom]

  def angstrom: Quantity[Rational, Angstrom] = Å

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
  implicit val WavelengthShow: Show[Wavelength] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val WavelengthOrd: Order[Wavelength] =
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

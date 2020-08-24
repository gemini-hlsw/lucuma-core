// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.Order
import cats.Show
import cats.implicits._
import coulomb._
import coulomb.cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import gsp.math.units._
import monocle.Iso
import monocle.Prism
import spire.math.Rational

/**
  * Exact wavelengths represented as positive integral picometers in the range (0 .. PosInt.MaxValue]
  * which means the largest representable wavelength is 2.147483647 mm.
  * @param toPicometers This wavelength in positive integral picometers (10^-12^ of a meter).
  */
final case class Wavelength(toPicometers: Quantity[PosInt, Picometer]) {

  /**
    * Returns the wavelength value in nanometers
    * The exact nanometer value needs to be represented as a Rational
    */
  def nm: Quantity[Rational, Nanometer] = toPicometers.to[Rational, Nanometer]

  def nanometer: Quantity[Rational, Nanometer] = nm

  /**
    * Returns the wavelength value in angstrom
    * The exact angstrom value needs to be represented as a Rational
    */
  def Å: Quantity[Rational, Angstrom] = toPicometers.to[Rational, Angstrom]

  def angstrom: Quantity[Rational, Angstrom] = Å

  override def toString: String =
    s"Wavelength(${toPicometers.show})"
}

object Wavelength {
  lazy val Min: Wavelength   = unsafeFromInt(1)
  lazy val Max: Wavelength   = unsafeFromInt(Int.MaxValue)
  // Max allowed value in nanometers
  lazy val MaxNanometer: Int = Int.MaxValue / BigInt(10).pow(3).toInt
  // Max allowed value in angstrom
  lazy val MaxAngstrom: Int  = Int.MaxValue / BigInt(10).pow(2).toInt

  /**
    * Construct a wavelength from a positive int
    * @group constructor
    */
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
    * Try to build a Wavelength with a value in nm in the range (0 .. 214783]
    * @group constructor
    */
  def fromNanometers(nm: Int): Option[Wavelength] =
    refineV[Positive](nm).toOption.flatMap(nm =>
      if (nm.value <= MaxNanometer) Wavelength(nm.withUnit[Nanometer]).some else none
    )

  /**
    * Try to build a Wavelength with a value in angstrom in the range (0 .. 2147]
    * @group constructor
    */
  def fromAngstrom(a: Int): Option[Wavelength] =
    refineV[Positive](a).toOption.flatMap(a =>
      if (a.value <= MaxAngstrom) Some(Wavelength(a.withUnit[Angstrom])) else None
    )

  /**
    * Iso from PosInt in pm into Wavelength and back.
    * @group Optics
    */
  val picometers: Iso[PosInt, Wavelength] =
    Iso[PosInt, Wavelength](i => Wavelength(i.withUnit[Picometer]))(_.toPicometers.value)

  /**
    * Prism from Int in pm into Wavelength and back.
    * @group Optics
    */
  val fromPicometers: Prism[Int, Wavelength] =
    Prism[Int, Wavelength](pm =>
      refineV[Positive](pm).toOption.map(v => Wavelength(v.withUnit[Picometer]))
    )(_.toPicometers.value)

}

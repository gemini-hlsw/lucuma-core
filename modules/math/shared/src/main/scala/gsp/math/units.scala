// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import coulomb._
import coulomb.si._
import coulomb.siprefix._
import coulomb.unitops.ConvertableUnits
import coulomb.unitops.UnitConverter
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import spire.math.Rational

trait units {
  // Wavelength units
  type Picometer            = Pico %* Meter
  type Nanometer            = Nano %* Meter
  type Angstrom             = Hecto %* Picometer
  type CentimetersPerSecond = (Centi %* Meter) %/ Second
  type MetersPerSecond      = Meter %/ Second
  type KilometersPerSecond  = (Kilo %* Meter) %/ Second

  // PositiveInt can be converted to Rational exactly
  implicit def rationalConverter[U1, U2](implicit
    cu: ConvertableUnits[U1, U2]
  ): UnitConverter[PosInt, U1, Rational, U2] =
    new UnitConverter[PosInt, U1, Rational, U2] {
      @inline def vcnv(v: PosInt): Rational =
        cu.coef * v.value
    }

  // This can build a converter for units that use PosInt but they are exact only
  // if the coef is more than 1 and whole, i.e. going from Nanometer to Picometer
  // The reverse is not true, remaining in the PosInt domain we can't ensure we can go from
  // Picometer to Nanometer without loosing precision
  // Thus we shouldn't make this implicit by default~
  def unsafePositiveIntConverter[U1, U2](implicit
    cu: ConvertableUnits[U1, U2]
  ): UnitConverter[PosInt, U1, PosInt, U2] =
    new UnitConverter[PosInt, U1, PosInt, U2] {
      @inline def vcnv(v: PosInt): PosInt =
        // We only allow the conversion if the coef is more than one and exact
        if (cu.coef.compareToOne > 0 && cu.coef.isWhole)
          // given the check above this should be positive and the refinement should always succeed
          refineV[Positive]((cu.coef * v.value).toInt).getOrElse(sys.error(s"Shouldn't happen"))
        else
          sys.error(s"Cannot convert exactly with coef ${cu.coef}")

    }

  // Implicit conversions that can be exact as Pico/Nano/Angstrom are multiples of 10
  implicit val convNP: UnitConverter[PosInt, Nanometer, PosInt, Picometer] =
    unsafePositiveIntConverter[Nanometer, Picometer]

  implicit val convAP: UnitConverter[PosInt, Angstrom, PosInt, Picometer] =
    unsafePositiveIntConverter[Angstrom, Picometer]

}

object units extends units

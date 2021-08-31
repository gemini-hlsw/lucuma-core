// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb._
import coulomb.accepted._
import coulomb.define.DerivedUnit
import coulomb.mks._
import coulomb.si._
import coulomb.siprefix._
import coulomb.time._
import coulomb.unitops.ConvertableUnits
import coulomb.unitops.UnitConverter
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import spire.math._

trait units {
  // Wavelength units
  type Picometer  = Pico %* Meter
  type Nanometer  = Nano %* Meter
  trait Angstrom
  implicit val defineAngstrom =
    DerivedUnit[Angstrom, Hecto %* Picometer](Rational.one, abbv = "Å")
  type Micrometer = Micro %* Meter

  trait CentimetersPerSecond
  implicit val defineUnitCentimetersPerSecond =
    DerivedUnit[CentimetersPerSecond, (Centi %* Meter) %/ Second](Rational.one, abbv = "cm/s")

  trait MetersPerSecond
  implicit val defineUnitMetersPerSecond =
    DerivedUnit[MetersPerSecond, Meter %/ Second](Rational(1), abbv = "m/s")

  trait KilometersPerSecond
  implicit val defineUnitKilometerPerSecond =
    DerivedUnit[KilometersPerSecond, Meter %/ Second](Rational(1000), abbv = "km/s")

  trait Year
  implicit val defineUnitYear =
    DerivedUnit[Year, Day](Rational(365), abbv = "y")

  private val DeciArcSecondsPerDegree: SafeLong  = 3600 * 10L
  private val MiliArcSecondsPerDegree: SafeLong  = 3600 * 1000L
  private val MicroArcSecondsPerDegree: SafeLong = 3600 * 1000000L

  trait DeciArcSecond
  implicit val defineUnitDeciArcSecond =
    DerivedUnit[DeciArcSecond, Degree](Rational(1, DeciArcSecondsPerDegree), abbv = "das")

  trait MilliArcSecond
  implicit val defineUnitMilliArcSecond =
    DerivedUnit[MilliArcSecond, Degree](Rational(1, MiliArcSecondsPerDegree), abbv = "mas")

  trait MicroArcSecond
  implicit val defineUnitMicroArcSecond =
    DerivedUnit[MicroArcSecond, Degree](Rational(1, MicroArcSecondsPerDegree), abbv = "μas")

  private val DaysPerYear: SafeLong = 365 * 8640L

  trait MilliArcSecondPerYear
  implicit val defineUnitMilliArcSecondPerYear =
    DerivedUnit[MilliArcSecondPerYear, Degree %/ Second](
      Rational(1, DaysPerYear * MiliArcSecondsPerDegree),
      abbv = "mas/y"
    )

  trait MicroArcSecondPerYear
  implicit val defineUnitMicroArcSecondPerYear =
    DerivedUnit[MicroArcSecondPerYear, Degree %/ Second](
      Rational(1L, DaysPerYear * MicroArcSecondsPerDegree),
      abbv = "μas/y"
    )

  // Magnitude system units
  private val JanskyPerWattMeter2Hertz: SafeLong = SafeLong(10).pow(26)
  trait Jansky
  implicit val defineJansky =
    DerivedUnit[Jansky, Watt %/ ((Meter %^ 2) %* (Hertz %^ -1))](Rational(1, JanskyPerWattMeter2Hertz), abbv = "Jy")

  private val ErgPerJoule: SafeLong = SafeLong(10).pow(7)
  trait Erg
  implicit val defineErg =
    DerivedUnit[Erg, Joule](Rational(1, ErgPerJoule), abbv = "erg")

  type WattsMag = Watt %/ ((Meter %^ 2) %* Micrometer)
  type ErgsWavelengthMag = Erg %/ (Second %* (Centimeter %^ 2) %* Angstrom)
  type ErgsFrequencyMag = Erg %/ (Second %* (Centimeter %^ 2) %* Hertz)

  // PosInt can be converted to Rational exactly
  implicit def rationalPosIntConverter[U1, U2](implicit
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
  def unsafePosIntConverter[U1, U2](implicit
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
    unsafePosIntConverter[Nanometer, Picometer]

  implicit val convAP: UnitConverter[PosInt, Angstrom, PosInt, Picometer] =
    unsafePosIntConverter[Angstrom, Picometer]

  implicit val convMP: UnitConverter[PosInt, Micrometer, PosInt, Picometer] =
    unsafePosIntConverter[Micrometer, Picometer]
}

object units extends units

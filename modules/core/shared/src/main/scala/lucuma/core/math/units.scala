// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb._
import coulomb.accepted._
import coulomb.define._
import coulomb.mks._
import coulomb.si._
import coulomb.siprefix._
import coulomb.time._
import coulomb.unitops.UnitString
import coulomb.unitops._
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.dimensional._
import spire.math._

trait units {

  type Pixels
  implicit val defineUnitPixels = BaseUnit[Pixels](abbv = "px")

  // Wavelength units
  type Picometer  = Pico %* Meter
  type Nanometer  = Nano %* Meter
  type Micrometer = Micro %* Meter
  type Angstrom
  implicit val defineAngstrom =
    DerivedUnit[Angstrom, Hecto %* Picometer](Rational.one, abbv = "Å")

  type MetersPerSecond      = Meter %/ Second
  type CentimetersPerSecond = (Centi %* Meter) %/ Second
  type KilometersPerSecond  = (Kilo %* Meter) %/ Second

  type Year
  implicit val defineUnitYear =
    DerivedUnit[Year, Day](Rational(365), abbv = "y")

  private val ArcSecondsPerDegree: SafeLong = 3600

  type ArcSecond
  implicit val defineUnitArcSecond =
    DerivedUnit[ArcSecond, Degree](Rational(1, ArcSecondsPerDegree), abbv = "arcsec")

  type DeciArcSecond  = Deci %* ArcSecond
  type MilliArcSecond = Milli %* ArcSecond
  type MicroArcSecond = Micro %* ArcSecond

  type MilliArcSecondPerYear = MilliArcSecond %/ Year
  type MicroArcSecondPerYear = MicroArcSecond %/ Year

  // Integrated Brightness units
  type VegaMagnitude
  implicit val defineVegaMagnitude =
    BaseUnit[VegaMagnitude](name = "Vega magnitude", abbv = "Vega mag")

  type ABMagnitude
  implicit val defineABMagnitude =
    BaseUnit[ABMagnitude](name = "AB magnitude", abbv = "AB mag")

  private val JanskysPerWattMeter2Hertz: SafeLong = SafeLong(10).pow(26)
  type Jansky
  implicit val defineJansky                       =
    DerivedUnit[Jansky, Watt %/ ((Meter %^ 2) %* (Hertz %^ -1))](
      Rational(1, JanskysPerWattMeter2Hertz),
      abbv = "Jy"
    )

  type WattsPerMeter2µMeter = Watt %/ ((Meter %^ 2) %* Micrometer)
  implicit val strWattsPerMeter2µMeter: UnitString[WattsPerMeter2µMeter] =
    UnitString[WattsPerMeter2µMeter]("W/m²/µm")

  private val ErgsPerJoule: SafeLong = SafeLong(10).pow(7)
  type Erg
  implicit val defineErg             =
    DerivedUnit[Erg, Joule](Rational(1, ErgsPerJoule), abbv = "erg")

  type ErgsPerSecondCentimeter2Angstrom = Erg %/ (Second %* (Centimeter %^ 2) %* Angstrom)
  implicit val strErgsPerSecondCentimeter2Angstrom: UnitString[ErgsPerSecondCentimeter2Angstrom] =
    UnitString[ErgsPerSecondCentimeter2Angstrom]("erg/s/cm²/Å")

  type ErgsPerSecondCentimeter2Hertz = Erg %/ (Second %* (Centimeter %^ 2) %* Hertz)
  implicit val strErgsPerSecondCentimeter2Hertz: UnitString[ErgsPerSecondCentimeter2Hertz] =
    UnitString[ErgsPerSecondCentimeter2Hertz]("erg/s/cm²/Hz")

  // Surface Brightness units
  type VegaMagnitudePerArcsec2 = VegaMagnitude %/ (ArcSecond %^ 2)
  implicit val strVegaMagnitudePerArcsec2: UnitString[VegaMagnitudePerArcsec2] =
    UnitString[VegaMagnitudePerArcsec2]("Vega mag/arcsec²")

  type ABMagnitudePerArcsec2 = ABMagnitude %/ (ArcSecond %^ 2)
  implicit val strABMagnitudePerArcsec2: UnitString[ABMagnitudePerArcsec2] =
    UnitString[ABMagnitudePerArcsec2]("AB mag/arcsec²")

  type JanskyPerArcsec2 = Jansky %/ (ArcSecond %^ 2)
  implicit val strJanskyPerArcsec2: UnitString[JanskyPerArcsec2] =
    UnitString[JanskyPerArcsec2]("Jy/arcsec²")

  type WattsPerMeter2µMeterArcsec2 = WattsPerMeter2µMeter %/ (ArcSecond %^ 2)
  implicit val strWattsPerMeter2µMeterArcsec2: UnitString[WattsPerMeter2µMeterArcsec2] =
    UnitString[WattsPerMeter2µMeterArcsec2]("W/m²/µm/arcsec²")

  type ErgsPerSecondCentimeter2AngstromArcsec2 =
    ErgsPerSecondCentimeter2Angstrom %/ (ArcSecond %^ 2)
  implicit val strErgsPerSecondCentimeter2AngstromArcsec2
    : UnitString[ErgsPerSecondCentimeter2AngstromArcsec2] =
    UnitString[ErgsPerSecondCentimeter2AngstromArcsec2]("erg/s/cm²/Å/arcsec²")

  type ErgsPerSecondCentimeter2HertzArcsec2 = ErgsPerSecondCentimeter2Hertz %/ (ArcSecond %^ 2)
  implicit val strErgsPerSecondCentimeter2HertzArcsec2
    : UnitString[ErgsPerSecondCentimeter2HertzArcsec2] =
    UnitString[ErgsPerSecondCentimeter2HertzArcsec2]("erg/s/cm²/Hz/arcsec²")

  // Integrated Line Flux units
  type WattsPerMeter2 = Watt %/ (Meter %^ 2)
  implicit val strWattsPerMeter2: UnitString[WattsPerMeter2] =
    UnitString[WattsPerMeter2]("W/m²")

  type ErgsPerSecondCentimeter2 = Erg %/ (Second %* (Centimeter %^ 2))
  implicit val strErgsPerSecondCentimeter2: UnitString[ErgsPerSecondCentimeter2] =
    UnitString[ErgsPerSecondCentimeter2]("erg/s/cm²")

  // Surface Line Flux units
  type WattsPerMeter2Arcsec2 = WattsPerMeter2 %/ (ArcSecond %^ 2)
  implicit val strWattsPerMeter2Arcsec2: UnitString[WattsPerMeter2Arcsec2] =
    UnitString[WattsPerMeter2Arcsec2]("W/m²/arcsec²")

  type ErgsPerSecondCentimeter2Arcsec2 = ErgsPerSecondCentimeter2 %/ (ArcSecond %^ 2)
  implicit val strErgsPerSecondCentimeter2Arcsec2: UnitString[ErgsPerSecondCentimeter2Arcsec2] =
    UnitString[ErgsPerSecondCentimeter2Arcsec2]("erg/s/cm²/arcsec²")

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

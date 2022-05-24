// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb._
import coulomb.accepted._
import coulomb.define._
import coulomb.mks._
import coulomb.si._
import coulomb.siprefix._
import coulomb.time._
import coulomb.unitops._
import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.dimensional._
import spire.math._

trait units {

  type Electron
  implicit val defineUnitElectron = BaseUnit[Electron](abbv = "e-")

  type Pixels
  implicit val defineUnitPixels = BaseUnit[Pixels](abbv = "px")

  // Wavelength units
  type Picometer  = Pico %* Meter
  type Nanometer  = Nano %* Meter
  type Micrometer = Micro %* Meter
  type Angstrom
  implicit val defineAngstrom =
    DerivedUnit[Angstrom, Hecto %* Picometer](Rational.one, abbv = "Å")

  type NanometersPerPixel = Nanometer %/ Pixels
  type PicometersPerPixel = Picometer %/ Pixels

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
    BaseUnitDef[VegaMagnitude]("Vega magnitude", "Vega mag", "VEGA_MAGNITUDE")

  type ABMagnitude
  implicit val defineABMagnitude =
    BaseUnitDef[ABMagnitude]("AB magnitude", "AB mag", "AB_MAGNITUDE")

  private val JanskysPerWattMeter2Hertz: SafeLong = SafeLong(10).pow(26)
  type Jansky
  implicit val defineJansky                       =
    DerivedUnitDef[Jansky, Watt %/ ((Meter %^ 2) %* (Hertz %^ -1))](
      Rational(1, JanskysPerWattMeter2Hertz),
      abbv = "Jy",
      serialized = "JANSKY"
    )

  type WattsPerMeter2Micrometer = Watt %/ ((Meter %^ 2) %* Micrometer)
  implicit val strWattsPerMeter2Micrometer =
    UnitStringDef[WattsPerMeter2Micrometer]("W/m²/µm", "W_PER_M_SQUARED_PER_UM")

  private val ErgsPerJoule: SafeLong = SafeLong(10).pow(7)
  type Erg
  implicit val defineErg             =
    DerivedUnit[Erg, Joule](Rational(1, ErgsPerJoule), abbv = "erg")

  type ErgsPerSecondCentimeter2Angstrom = Erg %/ (Second %* (Centimeter %^ 2) %* Angstrom)
  implicit val strErgsPerSecondCentimeter2Angstrom =
    UnitStringDef[ErgsPerSecondCentimeter2Angstrom]("erg/s/cm²/Å", "ERG_PER_S_PER_CM_SQUARED_PER_A")

  type ErgsPerSecondCentimeter2Hertz = Erg %/ (Second %* (Centimeter %^ 2) %* Hertz)
  implicit val strErgsPerSecondCentimeter2Hertz =
    UnitStringDef[ErgsPerSecondCentimeter2Hertz]("erg/s/cm²/Hz", "ERG_PER_S_PER_CM_SQUARED_PER_HZ")

  // Surface Brightness units
  type VegaMagnitudePerArcsec2 = VegaMagnitude %/ (ArcSecond %^ 2)
  implicit val strVegaMagnitudePerArcsec2 =
    UnitStringDef[VegaMagnitudePerArcsec2]("Vega mag/arcsec²", "VEGA_MAG_PER_ARCSEC_SQUARED")

  type ABMagnitudePerArcsec2 = ABMagnitude %/ (ArcSecond %^ 2)
  implicit val strABMagnitudePerArcsec2 =
    UnitStringDef[ABMagnitudePerArcsec2]("AB mag/arcsec²", "AB_MAG_PER_ARCSEC_SQUARED")

  type JanskyPerArcsec2 = Jansky %/ (ArcSecond %^ 2)
  implicit val strJanskyPerArcsec2 =
    UnitStringDef[JanskyPerArcsec2]("Jy/arcsec²", "JY_PER_ARCSEC_SQUARED")

  type WattsPerMeter2MicrometerArcsec2 = WattsPerMeter2Micrometer %/ (ArcSecond %^ 2)
  implicit val strWattsPerMeter2MicrometerArcsec2 =
    UnitStringDef[WattsPerMeter2MicrometerArcsec2](
      "W/m²/µm/arcsec²",
      "W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED"
    )

  type ErgsPerSecondCentimeter2AngstromArcsec2 =
    ErgsPerSecondCentimeter2Angstrom %/ (ArcSecond %^ 2)
  implicit val strErgsPerSecondCentimeter2AngstromArcsec2 =
    UnitStringDef[ErgsPerSecondCentimeter2AngstromArcsec2](
      "erg/s/cm²/Å/arcsec²",
      "ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED"
    )

  type ErgsPerSecondCentimeter2HertzArcsec2 = ErgsPerSecondCentimeter2Hertz %/ (ArcSecond %^ 2)
  implicit val strErgsPerSecondCentimeter2HertzArcsec2 =
    UnitStringDef[ErgsPerSecondCentimeter2HertzArcsec2](
      "erg/s/cm²/Hz/arcsec²",
      "ERG_PER_S_PER_CM_SQUARED_PER_HZ_PER_ARCSEC_SQUARED"
    )

  // Integrated Line Flux units
  type WattsPerMeter2 = Watt %/ (Meter %^ 2)
  implicit val strWattsPerMeter2 =
    UnitStringDef[WattsPerMeter2]("W/m²", "W_PER_M_SQUARED")

  type ErgsPerSecondCentimeter2 = Erg %/ (Second %* (Centimeter %^ 2))
  implicit val strErgsPerSecondCentimeter2 =
    UnitStringDef[ErgsPerSecondCentimeter2]("erg/s/cm²", "ERG_PER_S_PER_CM_SQUARED")

  // Surface Line Flux units
  type WattsPerMeter2Arcsec2 = WattsPerMeter2 %/ (ArcSecond %^ 2)
  implicit val strWattsPerMeter2Arcsec2 =
    UnitStringDef[WattsPerMeter2Arcsec2]("W/m²/arcsec²", "W_PER_M_SQUARED_PER_ARCSEC_SQUARED")

  type ErgsPerSecondCentimeter2Arcsec2 = ErgsPerSecondCentimeter2 %/ (ArcSecond %^ 2)
  implicit val strErgsPerSecondCentimeter2Arcsec2 =
    UnitStringDef[ErgsPerSecondCentimeter2Arcsec2](
      "erg/s/cm²/arcsec²",
      "ERG_PER_S_PER_CM_SQUARED_PER_ARCSEC_SQUARED"
    )

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

  // Integer Percents
  type ZeroTo100  = numeric.Interval.Closed[0, 100]
  type IntPercent = Int Refined ZeroTo100

  object IntPercent extends RefinedTypeOps[IntPercent, Int]
}

object units extends units

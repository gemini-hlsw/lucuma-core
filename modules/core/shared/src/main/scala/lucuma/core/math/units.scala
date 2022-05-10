// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb._
import coulomb.units.accepted._
import coulomb.define._
import coulomb.units.mks._
import coulomb.units.si._
import coulomb.units.si.prefixes._
import coulomb.units.time._
// import coulomb.unitops._
import lucuma.core.util.TypeString
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
// import lucuma.core.math.dimensional._
import _root_.spire.math._

trait units {

  type Pixels
  given BaseUnit[Pixels, "pixels", "px"] = BaseUnit()

  // // Wavelength units
  type Picometer  = Pico * Meter
  type Nanometer  = Nano * Meter
  type Micrometer = Micro * Meter
  type Angstrom
  given DerivedUnit[Angstrom, Hecto * Picometer, "angstrom", "Å"] = DerivedUnit()

  type NanometersPerPixel = Nanometer / Pixels
  type PicometersPerPixel = Picometer / Pixels

  type MetersPerSecond      = Meter / Second
  type CentimetersPerSecond = (Centi * Meter) / Second
  type KilometersPerSecond  = (Kilo * Meter) / Second

  type Year
  given DerivedUnit[Year, 365 * Day, "year", "y"] = DerivedUnit()

  private val ArcSecondsPerDegree: SafeLong = 3600

  type ArcSecond
  given DerivedUnit[ArcSecond, Degree / 3600, "arc second", "arcsec"] = DerivedUnit()

  type DeciArcSecond  = Deci * ArcSecond
  type MilliArcSecond = Milli * ArcSecond
  type MicroArcSecond = Micro * ArcSecond

  type MilliArcSecondPerYear = MilliArcSecond / Year
  type MicroArcSecondPerYear = MicroArcSecond / Year

  // Integrated Brightness units
  type VegaMagnitude
  given BaseUnit[VegaMagnitude, "Vega magnitude", "Vega mag"] = BaseUnit()
  given TypeString[VegaMagnitude] = TypeString("VEGA_MAGNITUDE")

  type ABMagnitude
  given BaseUnit[ABMagnitude, "AB magnitude", "AB mag"] = BaseUnit()
  given TypeString[ABMagnitude] = TypeString("AB_MAGNITUDE")

  type Jansky
  given DerivedUnit[Jansky, (10 ^ -26) * Watt / ((Meter ^ 2) * (Hertz ^ -1)), "Jansky", "Jy"] = DerivedUnit()
  given TypeString[Jansky] = TypeString("JANSKY")

  type WattsPerMeter2Micrometer = Watt / ((Meter ^ 2) * Micrometer)
  given TypeString[WattsPerMeter2Micrometer] = TypeString("W_PER_M_SQUARED_PER_UM")

  type Erg
  given DerivedUnit[Erg, (10 ^ -7) * Joule, "erg", "erg"] = DerivedUnit()

  type ErgsPerSecondCentimeter2Angstrom = Erg / (Second * (Centimeter ^ 2) * Angstrom)
  given TypeString[ErgsPerSecondCentimeter2Angstrom] = TypeString("ERG_PER_S_PER_CM_SQUARED_PER_A")

  type ErgsPerSecondCentimeter2Hertz = Erg / (Second * (Centimeter ^ 2) * Hertz)
  given TypeString[ErgsPerSecondCentimeter2Hertz] = TypeString("ERG_PER_S_PER_CM_SQUARED_PER_HZ")

  // Surface Brightness units
  type VegaMagnitudePerArcsec2 = VegaMagnitude / (ArcSecond ^ 2)
  given TypeString[VegaMagnitudePerArcsec2] = TypeString("VEGA_MAG_PER_ARCSEC_SQUARED")

  type ABMagnitudePerArcsec2 = ABMagnitude / (ArcSecond ^ 2)
  given TypeString[ABMagnitudePerArcsec2] = TypeString("AB_MAG_PER_ARCSEC_SQUARED")

  type JanskyPerArcsec2 = Jansky / (ArcSecond ^ 2)
  given TypeString[JanskyPerArcsec2] = TypeString("JY_PER_ARCSEC_SQUARED")

  type WattsPerMeter2MicrometerArcsec2 = WattsPerMeter2Micrometer / (ArcSecond ^ 2)
  given TypeString[WattsPerMeter2MicrometerArcsec2] = TypeString("W_PER_M_SQUARED_PER_UM_PER_ARCSEC_SQUARED")

  type ErgsPerSecondCentimeter2AngstromArcsec2 =
    ErgsPerSecondCentimeter2Angstrom / (ArcSecond ^ 2)
  given TypeString[ErgsPerSecondCentimeter2AngstromArcsec2] =
    TypeString("ERG_PER_S_PER_CM_SQUARED_PER_A_PER_ARCSEC_SQUARED")

  type ErgsPerSecondCentimeter2HertzArcsec2 = ErgsPerSecondCentimeter2Hertz / (ArcSecond ^ 2)
  given TypeString[ErgsPerSecondCentimeter2HertzArcsec2] =
    TypeString("ERG_PER_S_PER_CM_SQUARED_PER_HZ_PER_ARCSEC_SQUARED")

  // Integrated Line Flux units
  type WattsPerMeter2 = Watt / (Meter ^ 2)
  given TypeString[WattsPerMeter2] = TypeString("W_PER_M_SQUARED")

  type ErgsPerSecondCentimeter2 = Erg / (Second * (Centimeter ^ 2))
  given TypeString[ErgsPerSecondCentimeter2] = TypeString("ERG_PER_S_PER_CM_SQUARED")

  // Surface Line Flux units
  type WattsPerMeter2Arcsec2 = WattsPerMeter2 / (ArcSecond ^ 2)
  given TypeString[WattsPerMeter2Arcsec2] = TypeString("W_PER_M_SQUARED_PER_ARCSEC_SQUARED")

  type ErgsPerSecondCentimeter2Arcsec2 = ErgsPerSecondCentimeter2 / (ArcSecond ^ 2)
  given TypeString[ErgsPerSecondCentimeter2Arcsec2] =
    TypeString("ERG_PER_S_PER_CM_SQUARED_PER_ARCSEC_SQUARED")

  // // PosInt can be converted to Rational exactly
  // implicit def rationalPosIntConverter[U1, U2](implicit
  //   cu: ConvertableUnits[U1, U2]
  // ): UnitConverter[PosInt, U1, Rational, U2] =
  //   new UnitConverter[PosInt, U1, Rational, U2] {
  //     @inline def vcnv(v: PosInt): Rational =
  //       cu.coef * v.value
  //   }

  // // This can build a converter for units that use PosInt but they are exact only
  // // if the coef is more than 1 and whole, i.e. going from Nanometer to Picometer
  // // The reverse is not true, remaining in the PosInt domain we can't ensure we can go from
  // // Picometer to Nanometer without loosing precision
  // // Thus we shouldn't make this implicit by default~
  // def unsafePosIntConverter[U1, U2](implicit
  //   cu: ConvertableUnits[U1, U2]
  // ): UnitConverter[PosInt, U1, PosInt, U2] =
  //   new UnitConverter[PosInt, U1, PosInt, U2] {
  //     @inline def vcnv(v: PosInt): PosInt =
  //       // We only allow the conversion if the coef is more than one and exact
  //       if (cu.coef.compareToOne > 0 && cu.coef.isWhole)
  //         // given the check above this should be positive and the refinement should always succeed
  //         refineV[Positive]((cu.coef * v.value).toInt).getOrElse(sys.error(s"Shouldn't happen"))
  //       else
  //         sys.error(s"Cannot convert exactly with coef ${cu.coef}")

  //   }

  // // Implicit conversions that can be exact as Pico/Nano/Angstrom are multiples of 10
  // implicit val convNP: UnitConverter[PosInt, Nanometer, PosInt, Picometer] =
  //   unsafePosIntConverter[Nanometer, Picometer]

  // implicit val convAP: UnitConverter[PosInt, Angstrom, PosInt, Picometer] =
  //   unsafePosIntConverter[Angstrom, Picometer]

  // implicit val convMP: UnitConverter[PosInt, Micrometer, PosInt, Picometer] =
  //   unsafePosIntConverter[Micrometer, Picometer]
}

object units extends units

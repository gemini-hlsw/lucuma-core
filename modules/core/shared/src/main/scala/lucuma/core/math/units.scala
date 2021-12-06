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
import coulomb.unitops.ConvertableUnits
import coulomb.unitops.UnitConverter
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.dimensional._
import spire.math._

trait units {

  trait Pixels
  implicit val defineUnitPixels = BaseUnit[Pixels](abbv = "px")

  // Wavelength units
  type Picometer = Pico %* Meter
  type Nanometer = Nano %* Meter
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

  // Integrated Brightness units
  type VegaMagnitude
  implicit val defineVegaMagnitude =
    PrefixUnit[VegaMagnitude](name = "Vega magnitudes", abbv = "Vega mags")

  type ABMagnitude
  implicit val defineABMagnitude =
    PrefixUnit[ABMagnitude](name = "AB magnitudes", abbv = "AB mags")

  private val JanskyPerWattMeter2Hertz: SafeLong = SafeLong(10).pow(26)
  type Jansky
  implicit val defineJansky                      =
    DerivedUnit[Jansky, Watt %/ ((Meter %^ 2) %* (Hertz %^ -1))](
      Rational(1, JanskyPerWattMeter2Hertz),
      abbv = "Jy"
    )

  type WattsBrightness
  implicit val defineWattsBrightness =
    DerivedUnit[WattsBrightness, Watt %/ ((Meter %^ 2) %* Micrometer)](abbv = "W/m²/µm")

  private val ErgPerJoule: SafeLong = SafeLong(10).pow(7)
  type Erg
  implicit val defineErg            =
    DerivedUnit[Erg, Joule](Rational(1, ErgPerJoule), abbv = "erg")

  type ErgsWavelengthBrightness
  implicit val defineErgsWavelengthBrightness =
    DerivedUnit[ErgsWavelengthBrightness, Erg %/ (Second %* (Centimeter %^ 2) %* Angstrom)](
      abbv = "erg/s/cm²/Å"
    )

  type ErgsFrequencyBrightness
  implicit val defineErgsFrequencyBrightness =
    DerivedUnit[ErgsFrequencyBrightness, Erg %/ (Second %* (Centimeter %^ 2) %* Hertz)](
      abbv = "erg/s/cm²/Hz"
    )

  // Surface Brightness units

  // Derive a surface unit from an integrated unit
  private def defineSurfaceUnit[U, D](implicit unitD: UnitOfMeasure[D]) =
    DerivedUnit[U, D %/ (ArcSecond %^ 2)](
      name = unitD.definition.name + "per arcsec²",
      abbv = unitD.definition.abbv + "/arcsec²"
    )

  type VegaMagnitudePerArcsec2
  implicit val defineVegaMagnitudePerArcsec2 =
    defineSurfaceUnit[VegaMagnitudePerArcsec2, VegaMagnitude]

  type ABMagnitudePerArcsec2
  implicit val defineABMagnitudePerArcsec2 =
    defineSurfaceUnit[ABMagnitudePerArcsec2, ABMagnitude]

  type JanskyPerArcsec2
  implicit val defineJanskyPerArcsec2 =
    defineSurfaceUnit[JanskyPerArcsec2, Jansky]

  type WattsBrightnessPerArcsec2
  implicit val defineWattsBrightnessPerArcsec2 =
    defineSurfaceUnit[WattsBrightnessPerArcsec2, WattsBrightness]

  type ErgsWavelengthBrightnessPerArcsec2
  implicit val defineErgsWavelengthBrightnessPerArcsec2 =
    defineSurfaceUnit[ErgsWavelengthBrightnessPerArcsec2, ErgsWavelengthBrightness]

  type ErgsFrequencyBrightnessPerArcsec2
  implicit val defineErgsFrequencyBrightnessPerArcsec2 =
    defineSurfaceUnit[ErgsFrequencyBrightnessPerArcsec2, ErgsFrequencyBrightness]

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

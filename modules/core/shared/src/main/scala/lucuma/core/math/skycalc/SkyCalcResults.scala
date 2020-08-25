// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats.Eq
import cats.Show
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import monocle.Getter

/** Struct that holds results of SkyCalc. */
final case class SkyCalcResults protected[skycalc](
  protected[skycalc] val altitudeRaw:         Double,
  protected[skycalc] val azimuthRaw:          Double,
  protected[skycalc] val parallacticAngleRaw: Double,
  airmass:                                    Double,
  protected[skycalc] val hourAngleRaw:        Double,
  lunarIlluminatedFraction:                   Float,
  lunarSkyBrightness:                         Double,
  totalSkyBrightness:                         Double,
  protected[skycalc] val lunarPhaseAngleRaw:  Double,
  protected[skycalc] val sunAltitudeRaw:      Double,
  lunarDistance:                              Double,
  protected[skycalc] val lunarElevationRaw:   Double
) {
  val altitude: Declination       = Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(altitudeRaw))._1
  val azimuth: Angle              = Angle.fromDoubleDegrees(azimuthRaw)
  val parallacticAngle: Angle     = Angle.fromDoubleDegrees(parallacticAngleRaw)
  val hourAngle: HourAngle        = HourAngle.fromDoubleHours(hourAngleRaw)
  val lunarPhaseAngle: Angle      = Angle.fromDoubleDegrees(lunarPhaseAngleRaw)
  val sunAltitude: Declination    = Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(sunAltitudeRaw))._1
  val lunarElevation: Declination = Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(lunarElevationRaw))._1
}

object SkyCalcResults {
  /** @group Typeclass Instances */
  implicit val SkyCalcResultsEqual: Eq[SkyCalcResults] = Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit val SkyCalcResultsShow: Show[SkyCalcResults] = Show.fromToString

  /** @group Optics */
  val altitude: Getter[SkyCalcResults, Declination] =
    Getter(_.altitude)

  /** @group Optics */
  val azimuth: Getter[SkyCalcResults, Angle] =
    Getter(_.azimuth)

  /** @group Optics */
  val parallacticAngle: Getter[SkyCalcResults, Angle] =
    Getter(_.parallacticAngle)

  /** @group Optics */
  val airmass: Getter[SkyCalcResults, Double] =
    Getter(_.airmass)

  /** @group Optics */
  val hourAngle: Getter[SkyCalcResults, HourAngle] =
    Getter(_.hourAngle)

  /** @group Optics */
  val lunarIlluminatedFraction: Getter[SkyCalcResults, Float] =
    Getter(_.lunarIlluminatedFraction)

  /** @group Optics */
  val lunarSkyBrightness: Getter[SkyCalcResults, Double] =
    Getter(_.lunarSkyBrightness)

  /** @group Optics */
  val totalSkyBrightness: Getter[SkyCalcResults, Double] =
    Getter(_.totalSkyBrightness)

  /** @group Optics */
  val lunarPhaseAngle: Getter[SkyCalcResults, Angle] =
    Getter(_.lunarPhaseAngle)

  /** @group Optics */
  val sunAltitude: Getter[SkyCalcResults, Declination] =
    Getter(_.sunAltitude)

  /** @group Optics */
  val lunarDistance: Getter[SkyCalcResults, Double] =
    Getter(_.lunarDistance)

  /** @group Optics */
  val lunarElevation: Getter[SkyCalcResults, Declination] =
    Getter(_.lunarElevation)
}

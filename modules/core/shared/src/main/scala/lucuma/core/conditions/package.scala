// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.conditions

import coulomb.Quantity
import coulomb.units.accepted.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric.Interval as RefinedInterval
import eu.timepit.refined.numeric.Less
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.math.erf
import lucuma.core.util.RefinedNewType
import lucuma.refined.*

import scala.math.Pi
import scala.math.abs
import scala.math.pow
import scala.math.sin

// Calculations of likelihood of occurrence of observing conditions
// Taken from:
// https://github.com/andrewwstephens/pyexplore/blob/3edd50f6c41509752cda6ad493ccaadd5eb5ad82/test/percentile.py
//
// Store a percentage with two decimals of precision for a 0-100% range
type CentiPercent    = RefinedInterval.Closed[0, 10000]
object IntCentiPercent extends RefinedNewType[Int, CentiPercent]:
  val Max = IntCentiPercent.unsafeFrom(10000)
  val Min = IntCentiPercent.unsafeFrom(0)

  /**
    * Approximate a double to a IntCentiPercent
    */
  def fromDouble(d: Double): IntCentiPercent = IntCentiPercent.unsafeFrom((d * 100).toInt)

  /**
    * Approximate a BigDecimal to a IntCentiPercent
    */
  def fromBigDecimal(d: BigDecimal): IntCentiPercent = IntCentiPercent.unsafeFrom((d * 100).toInt)

  extension(a: IntCentiPercent)
    def toPercent: Double = a.value.value / 100.0
    def *(b: IntCentiPercent): IntCentiPercent =
      // Given a and b are in the range 0-1 the result is also in the range 0-1
      IntCentiPercent.unsafeFrom((a.value.value * b.value.value) / 10000)
    def round: IntCentiPercent = IntCentiPercent.unsafeFrom(100 * scala.math.round(a.value.value / 100.0f))

type IntCentiPercent = IntCentiPercent.Type

type AirMassPredicate = Not[Less[1]]
type AirMassValue     = BigDecimal Refined AirMassPredicate

/**
  * Return the minimum airmass at a certain declination at a site.
  */
def minimumAirmass(dec: Declination, site: Site): AirMassValue =
  val latitude = site.place.latitude
  // Maximum elevation in degrees
  val elevation = 90.0 - abs(dec.toAngle.toSignedDoubleDegrees - latitude.toAngle.toSignedDoubleDegrees)
  refineV[AirMassPredicate](BigDecimal(1.0 / sin((elevation + 244.0 / (165.0 + 47.0 * pow(elevation, 1.1))) * Pi / 180.0))).getOrElse(sys.error("Not possible"))

/**
  * Return the percentile of the sky brightness.
  * https://www.gemini.edu/observing/telescopes-and-sites/sites#SkyBackground
  */
def percentileSkyBackground(bg: SkyBackground): IntCentiPercent =
  bg match
    case SkyBackground.Darkest => IntCentiPercent(2000.refined)
    case SkyBackground.Dark    => IntCentiPercent(5000.refined)
    case SkyBackground.Gray    => IntCentiPercent(8000.refined)
    case SkyBackground.Bright  => IntCentiPercent(10000.refined)

/**
  * Return the percentile of a measured extinction.
  * extinction in magnitudes
  */
def percentileCloudCoverage(extinction: CloudExtinction): IntCentiPercent =
  extinction match
    case CloudExtinction.PointOne       => IntCentiPercent(5000.refined)
    case CloudExtinction.PointThree     => IntCentiPercent(7000.refined)
    case CloudExtinction.PointFive      => IntCentiPercent(7500.refined)
    case CloudExtinction.OnePointZero   => IntCentiPercent(8000.refined)
    case CloudExtinction.OnePointFive   => IntCentiPercent(9000.refined)
    case CloudExtinction.TwoPointZero   => IntCentiPercent(9500.refined)
    case CloudExtinction.ThreePointZero => IntCentiPercent(10000.refined)

/**
  * Return the percentile of the water vapor.
  * https://www.gemini.edu/observing/telescopes-and-sites/sites#SkyTransparencyWater
  */
def percentileWaterVapor(wv: WaterVapor): IntCentiPercent =
  wv match
    case WaterVapor.VeryDry  => IntCentiPercent(2000.refined)
    case WaterVapor.Dry      => IntCentiPercent(5000.refined)
    case WaterVapor.Median   => IntCentiPercent(8000.refined)
    case WaterVapor.Wet      => IntCentiPercent(10000.refined)

/**
  * Calculate the percentile of on-source image quality.
  * fwhm in arcsec (on-source) wavelength in microns
  */
def percentileImageQuality(fwhm: Quantity[BigDecimal, ArcSecond], wavelength: Wavelength, airmass: AirMassValue): IntCentiPercent =
  val zenithFwhm = fwhm.value.toDouble / pow(airmass.value.toDouble, 0.6)

  // model fit to QAP from 2004-2024:  (the extra +0.5 is to force 100% in the worst IQ)
  val c = Array(50.10221383, 0.87712202, 0.78467697, 16.10928544, 0.13778389, -15.8255612, 49.37405633 + 0.5)
  IntCentiPercent.fromDouble(c(0) * erf(c(1) * pow(wavelength.toMicrometers.value.value.toDouble, c(2)) + c(3) * pow(zenithFwhm, c(4)) + c(5)) + c(6))

def conditionsLikelihood(bg: SkyBackground, extinction: CloudExtinction, wv: WaterVapor, fwhm: Quantity[BigDecimal, ArcSecond], wavelength: Wavelength, dec: Declination, site: Site): IntCentiPercent =
    (percentileSkyBackground(bg) *
      percentileCloudCoverage(extinction)  *
      percentileImageQuality(fwhm, wavelength, minimumAirmass(dec, site)) *
      percentileWaterVapor(wv)).round


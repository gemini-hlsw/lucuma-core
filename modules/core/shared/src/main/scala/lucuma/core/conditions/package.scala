// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.conditions

import coulomb.Quantity
import coulomb.units.accepted.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.math.erf
import lucuma.core.model.AirMassPredicate
import lucuma.core.model.AirMass
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Percentile

import scala.math.Pi
import scala.math.abs
import scala.math.pow
import scala.math.sin

// Calculations of likelihood of occurrence of observing conditions
// Taken from:
// https://github.com/andrewwstephens/pyexplore/blob/3edd50f6c41509752cda6ad493ccaadd5eb5ad82/test/percentile.py
//

/**
  * Return the minimum airmass at a certain declination at a site.
  */
def minimumAirmass(dec: Declination, site: Site): AirMass =
  val latitude = site.place.latitude
  // Maximum elevation in degrees
  val elevation = 90.0 - abs(dec.toAngle.toSignedDoubleDegrees - latitude.toAngle.toSignedDoubleDegrees)
  AirMass.from(BigDecimal(1.0 / sin((elevation + 244.0 / (165.0 + 47.0 * pow(elevation, 1.1))) * Pi / 180.0))).getOrElse(sys.error("Not possible"))

/**
  * Calculate the percentile of on-source image quality.
  * fwhm in arcsec (on-source) wavelength in microns
  */
def percentileImageQuality(fwhm: Quantity[BigDecimal, ArcSecond], wavelength: Wavelength, airmass: AirMass): Percentile =
  val zenithFwhm = fwhm.value.toDouble / pow(airmass.value.value.toDouble, 0.6)

  // model fit to QAP from 2004-2024:  (the extra +0.5 is to force 100% in the worst IQ)
  val c = Array(50.10221383, 0.87712202, 0.78467697, 16.10928544, 0.13778389, -15.8255612, 49.37405633 + 0.5)
  // The equation should give a number between 0 and 100 but rounding can give numbers below 0 or above 100. It is clamped to that range.
  val result = c(0) * erf(c(1) * pow(wavelength.toMicrometers.value.value.toDouble, c(2)) + c(3) * pow(zenithFwhm, c(4)) + c(5)) + c(6)
  Percentile.FromBigDecimal.getOption(result).getOrElse {
    if (result < 0) Percentile.Min
    else Percentile.Max
  }

def conditionsLikelihood(bg: SkyBackground, ce: CloudExtinction.Preset, wv: WaterVapor, fwhm: Quantity[BigDecimal, ArcSecond], wavelength: Wavelength, dec: Declination, site: Site): Percentile =
    (bg.percentile *
      ce.percentile  *
      percentileImageQuality(fwhm, wavelength, minimumAirmass(dec, site)) *
      wv.percentile).rounded


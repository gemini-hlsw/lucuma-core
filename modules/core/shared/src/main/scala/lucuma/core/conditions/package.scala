// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.conditions

import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.IntCentiPercent

// Calculations of likelihood of occurrence of observing conditions
// Taken from:
// https://github.com/andrewwstephens/pyexplore/blob/3edd50f6c41509752cda6ad493ccaadd5eb5ad82/test/percentile.py
//
def conditionsLikelihood(bg: SkyBackground, ce: CloudExtinction, wv: WaterVapor, fwhm: ImageQuality, wavelength: Wavelength, dec: Declination, site: Site): IntCentiPercent =
    (bg.percentile *
      ce.percentile  *
      fwhm.percentile(wavelength, site.minimumAirMassFor(dec)) *
      wv.percentile).round


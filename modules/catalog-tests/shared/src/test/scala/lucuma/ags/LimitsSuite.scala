// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import lucuma.catalog.BandsList
import lucuma.catalog.BrightnessConstraints
import lucuma.catalog.FaintnessConstraint
import lucuma.catalog.SaturationConstraint
import lucuma.core.enums.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange

class LimitsSuite extends munit.FunSuite {
  test("widestConstraints") {
    assertEquals(
      widestConstraints,
      BrightnessConstraints(BandsList.GaiaBandsList,
                            FaintnessConstraint(BrightnessValue.unsafeFrom(17.227769563884202)),
                            None
      )
    )
  }

  test("gaia brightness constraints") {
    // Sample query from explore

    val constraints = ConstraintSet(
      ImageQuality.PointOne,         // min image quality
      CloudExtinction.PointOne,      // min cloud extinction
      SkyBackground.Dark,            // Not relevant
      WaterVapor.Wet,                // Not relevant
      ElevationRange.AirMass.Default // Not relevant
    )

    val wavelength = Wavelength.fromIntNanometers(300).get

    val brightnessConstraints =
      gaiaBrightnessConstraints(constraints, GuideSpeed.Fast, wavelength)

    assertEquals(
      brightnessConstraints,
      BrightnessConstraints(
        BandsList.GaiaBandsList,
        FaintnessConstraint(BrightnessValue.unsafeFrom(16.127769563884204)),
        Some(SaturationConstraint(BrightnessValue.unsafeFrom(10.127769563884204)))
      )
    )
  }
}

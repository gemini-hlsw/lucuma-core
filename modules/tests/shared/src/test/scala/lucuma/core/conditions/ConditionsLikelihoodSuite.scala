// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.conditions

import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMass
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality

/**
  * Tests the likelihood calculations
  */
class ConditionsLikelihoodSuite extends munit.FunSuite:

  test("minimum airmass"):
    assertEqualsDouble(Site.GN.minimumAirMassFor(Declination.fromDoubleDegrees(20).get).value.value.toDouble, 1.000003, 0.000001)
    assertEqualsDouble(Site.GN.minimumAirMassFor(Declination.fromDoubleDegrees(80).get).value.value.toDouble, 2.003746, 0.000001)
    assertEqualsDouble(Site.GS.minimumAirMassFor(Declination.fromDoubleDegrees(-30).get).value.value.toDouble, 1.000006, 0.000001)
    assertEqualsDouble(Site.GS.minimumAirMassFor(Declination.fromDoubleDegrees(-90).get).value.value.toDouble, 1.978889, 0.000001)

  test("percentile sky background"):
    assertEquals(SkyBackground.Bright.percentile.toPercent.toDouble, 100.0)
    assertEquals(SkyBackground.Dark.percentile.toPercent.toDouble, 50.0)

  test("percentile cloud coverage"):
    assertEquals(CloudExtinction.Preset.PointOne.percentile.toPercent.toDouble, 50.0)
    assertEquals(CloudExtinction.Preset.PointThree.percentile.toPercent.toDouble, 70.0)
    assertEquals(CloudExtinction.Preset.ThreePointZero.percentile.toPercent.toDouble, 100.0)

  test("percentile water vapor"):
    assertEquals(WaterVapor.Dry.percentile.toPercent.toDouble, 50.0)
    assertEquals(WaterVapor.Wet.percentile.toPercent.toDouble, 100.0)

  test("percentile image quality"):
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(0.5).percentile(Wavelength.fromIntNanometers(630).get, AirMass.from(BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 20.69, 0.00001)  // r IQ20 @ AM1
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(0.75).percentile(Wavelength.fromIntNanometers(630).get, AirMass.from(BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 64.67, 0.00001)   // r IQ70
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(1.05).percentile(Wavelength.fromIntNanometers(630).get, AirMass.from(BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 92.15, 0.00001)   // r IQ85
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(1.8).percentile(Wavelength.fromIntNanometers(630).get, AirMass.from(BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 99.90, 0.00001)   // r IQany
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(2.0).percentile(Wavelength.fromIntNanometers(2200).get, AirMass.from(BigDecimal(2)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 99.95, 0.00001)
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(0.5).percentile(Wavelength.fromIntNanometers(630).get, AirMass.from(BigDecimal(2)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 2.25, 0.00001)
    assertEqualsDouble(ImageQuality.unsafeFromArcSeconds(0.1).percentile(Wavelength.fromIntNanometers(630).get, AirMass.from(BigDecimal(2)).getOrElse(sys.error("Not possible"))).toPercent.toDouble, 0.0, 0.00001)

  test("conditions likelihood"):
    assertEquals(conditionsLikelihood(SkyBackground.Bright, CloudExtinction.unsafeFromVegaMagnitude(3.0), WaterVapor.Wet, ImageQuality.unsafeFromArcSeconds(3.0), Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent.toDouble, 100.00)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.unsafeFromVegaMagnitude(3.0), WaterVapor.Wet, ImageQuality.unsafeFromArcSeconds(3.0), Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent.toDouble, 50.00)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.unsafeFromVegaMagnitude(0.1), WaterVapor.Wet, ImageQuality.unsafeFromArcSeconds(3.0), Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent.toDouble, 25.00)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.unsafeFromVegaMagnitude(0.1), WaterVapor.Dry, ImageQuality.unsafeFromArcSeconds(3.0), Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent.toDouble, 12.00)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.unsafeFromVegaMagnitude(0.1), WaterVapor.Wet, ImageQuality.unsafeFromArcSeconds(0.5), Wavelength.fromIntNanometers(500).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent.toDouble, 4.00)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.unsafeFromVegaMagnitude(0.1), WaterVapor.Wet, ImageQuality.unsafeFromArcSeconds(0.5), Wavelength.fromIntNanometers(500).get, Declination.fromDoubleDegrees(68).get, Site.GN).toPercent.toDouble, 1.00)

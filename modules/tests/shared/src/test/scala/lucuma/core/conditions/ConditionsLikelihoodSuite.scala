// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.conditions

import coulomb.syntax.*
import coulomb.units.accepted.*
import eu.timepit.refined.refineV
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Declination
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMassPredicate
import lucuma.core.model.CloudExtinction

/**
  * Tests the likelihood calculations
  */
class ConditionsLikelihoodSuite extends munit.FunSuite:

  test("minimum airmass"):
    assertEqualsDouble(minimumAirmass(Declination.fromDoubleDegrees(20).get, Site.GN).value.toDouble, 1.000003, 0.000001)
    assertEqualsDouble(minimumAirmass(Declination.fromDoubleDegrees(80).get, Site.GN).value.toDouble, 2.003746, 0.000001)
    assertEqualsDouble(minimumAirmass(Declination.fromDoubleDegrees(-30).get, Site.GS).value.toDouble, 1.000006, 0.000001)
    assertEqualsDouble(minimumAirmass(Declination.fromDoubleDegrees(-90).get, Site.GS).value.toDouble, 1.978889, 0.000001)

  test("percentile sky background"):
    assertEquals(percentileSkyBackground(SkyBackground.Bright).toPercent, 100.0)
    assertEquals(percentileSkyBackground(SkyBackground.Dark).toPercent, 50.0)

  test("percentile cloud coverage"):
    assertEquals(percentileCloudCoverage(CloudExtinction.Point.PointOne).toPercent, 50.0)
    assertEquals(percentileCloudCoverage(CloudExtinction.Point.PointThree).toPercent, 70.0)
    assertEquals(percentileCloudCoverage(CloudExtinction.Point.ThreePointZero).toPercent, 100.0)

  test("percentile water vapor"):
    assertEquals(percentileWaterVapor(WaterVapor.Dry).toPercent, 50.0)
    assertEquals(percentileWaterVapor(WaterVapor.Wet).toPercent, 100.0)

  test("percentile image quality"):
    assertEqualsDouble(percentileImageQuality(BigDecimal(0.5).withUnit[ArcSecond], Wavelength.fromIntNanometers(630).get, refineV[AirMassPredicate](BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent, 20.69, 0.00001)  // r IQ20 @ AM1
    assertEqualsDouble(percentileImageQuality(BigDecimal(0.75).withUnit[ArcSecond], Wavelength.fromIntNanometers(630).get, refineV[AirMassPredicate](BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent, 64.67, 0.00001)   // r IQ70
    assertEqualsDouble(percentileImageQuality(BigDecimal(1.05).withUnit[ArcSecond], Wavelength.fromIntNanometers(630).get, refineV[AirMassPredicate](BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent, 92.15, 0.00001)   // r IQ85
    assertEqualsDouble(percentileImageQuality(BigDecimal(1.8).withUnit[ArcSecond], Wavelength.fromIntNanometers(630).get, refineV[AirMassPredicate](BigDecimal(1)).getOrElse(sys.error("Not possible"))).toPercent, 99.90, 0.00001)   // r IQany
    assertEqualsDouble(percentileImageQuality(BigDecimal(2.0).withUnit[ArcSecond], Wavelength.fromIntNanometers(2200).get, refineV[AirMassPredicate](BigDecimal(2)).getOrElse(sys.error("Not possible"))).toPercent, 99.95, 0.00001)
    assertEqualsDouble(percentileImageQuality(BigDecimal(0.5).withUnit[ArcSecond], Wavelength.fromIntNanometers(630).get, refineV[AirMassPredicate](BigDecimal(2)).getOrElse(sys.error("Not possible"))).toPercent, 2.25, 0.00001)
    assertEqualsDouble(percentileImageQuality(BigDecimal(0.1).withUnit[ArcSecond], Wavelength.fromIntNanometers(630).get, refineV[AirMassPredicate](BigDecimal(2)).getOrElse(sys.error("Not possible"))).toPercent, 0.0, 0.00001)

  test("conditions likelihood"):
    assertEquals(conditionsLikelihood(SkyBackground.Bright, CloudExtinction.Point.ThreePointZero, WaterVapor.Wet, BigDecimal(3).withUnit[ArcSecond], Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent, 100.0)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.Point.ThreePointZero, WaterVapor.Wet, BigDecimal(3).withUnit[ArcSecond], Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent, 50.0)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.Point.PointOne, WaterVapor.Wet, BigDecimal(3).withUnit[ArcSecond], Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent, 25.0)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.Point.PointOne, WaterVapor.Dry, BigDecimal(3).withUnit[ArcSecond], Wavelength.fromIntNanometers(1000).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent, 12.0)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.Point.PointOne, WaterVapor.Wet, BigDecimal(0.5).withUnit[ArcSecond], Wavelength.fromIntNanometers(500).get, Declination.fromDoubleDegrees(20).get, Site.GN).toPercent, 4.0)
    assertEquals(conditionsLikelihood(SkyBackground.Dark, CloudExtinction.Point.PointOne, WaterVapor.Wet, BigDecimal(0.5).withUnit[ArcSecond], Wavelength.fromIntNanometers(500).get, Declination.fromDoubleDegrees(68).get, Site.GN).toPercent, 1.0)

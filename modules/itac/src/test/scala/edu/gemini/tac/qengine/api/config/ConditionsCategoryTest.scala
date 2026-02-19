// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import cats.implicits.*
import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.p1.*
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import munit.FunSuite

import Cat.*

class ConditionsCategoryTest extends FunSuite {
  private val oc = ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.PointThree, SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)

  test("testEq") {
    val matches = CloudExtinction.Preset.values.toList.map(x => Cat(ccSpec=Eq(x)).matches(oc))
    assertEquals(List(false, false, true, false, false, false, false), matches)
  }

  test("testLe") {
    val matches = CloudExtinction.Preset.values.toList.map(x => Cat(ccSpec=Le(x)).matches(oc))
    assertEquals(List(false, false, true, true, true, true, true), matches)
  }

  test("testGe") {
    val matches = CloudExtinction.Preset.values.toList.map(x => Cat(ccSpec=Ge(x)).matches(oc))
    assertEquals(List(true, true, true, false, false, false, false), matches)
  }

  test("testMultiple") {
    val cat = Cat(Eq(CloudExtinction.Preset.PointThree), Eq(ImageQuality.Preset.PointOne))
    assert(cat.matches(oc))
    assert(!cat.matches(ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.Zero, SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)))
    assert(!cat.matches(ObservingConditions(ImageQuality.Preset.OnePointZero, CloudExtinction.Preset.PointThree, SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)))
  }

  test("testAll") {
    val cat = Cat(Eq(CloudExtinction.Preset.PointThree), Eq(ImageQuality.Preset.PointOne), Eq(SkyBackground.Darkest), Eq(WaterVapor.VeryDry))
    assert(cat.matches(oc))
    assert(!cat.matches(ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.PointThree, SkyBackground.Dark, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)))
    assert(!cat.matches(ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.PointThree, SkyBackground.Darkest, WaterVapor.Dry, ElevationRange.ByAirMass.Default)))
  }

  test("testUnspecified") {
    val cat = Cat()
    assert(cat.matches(oc))
    assert(cat.matches(ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.PointThree, SkyBackground.Dark, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)))
    assert(cat.matches(ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.PointThree, SkyBackground.Darkest, WaterVapor.Dry, ElevationRange.ByAirMass.Default)))
  }

  test("testCanObserveEq") {
    val cat0 = Eq[CloudExtinction.Preset](CloudExtinction.Preset.Zero)
    val cat1 = Eq[CloudExtinction.Preset](CloudExtinction.Preset.PointThree)
    assert(cat0.canObserve(CloudExtinction.Preset.Zero))
    assert(cat0.canObserve(CloudExtinction.Preset.PointThree))
    assert(!cat1.canObserve(CloudExtinction.Preset.Zero))
    assert(cat0.canObserve(CloudExtinction.Preset.PointThree))
  }

  test("testCanObserveLe") {
    val cat = Le[SkyBackground](SkyBackground.Dark)
    SkyBackground.values foreach { sb => assert(cat.canObserve(sb)) }
  }

  test("testCanObserveGe") {
    val cat = Ge[CloudExtinction.Preset](CloudExtinction.Preset.PointThree)
    List(CloudExtinction.Preset.PointThree, CloudExtinction.Preset.OnePointZero, CloudExtinction.Preset.ThreePointZero) foreach { cc => assert(cat.canObserve(cc)) }
    assert(!cat.canObserve(CloudExtinction.Preset.Zero))
  }

  test("testCanObserveAll") {
    val cat1 = Cat(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.PointOne), Le(SkyBackground.Dark))
    val cat2 = Cat(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.PointOne), Ge(SkyBackground.Gray))
    val cat3 = Cat(Ge(CloudExtinction.Preset.PointThree), Eq(ImageQuality.Preset.PointOne))

    val oc = ObservingConditions(ImageQuality.Preset.PointOne, CloudExtinction.Preset.OnePointZero, SkyBackground.Dark, WaterVapor.Wet, ElevationRange.ByAirMass.Default)

    assert(cat3.canObserve(oc))
    assert(!cat2.canObserve(oc))
    assert(cat1.canObserve(oc))

    val sp = SearchPath(List(cat1, cat2, cat3))
    assertEquals(List(cat3, cat1), sp(oc))
  }
}
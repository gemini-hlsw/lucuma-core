// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.ImageQuality.*
import edu.gemini.tac.qengine.p1.SkyBackground.*
import edu.gemini.tac.qengine.p1.WaterVapor.*
import munit.FunSuite
import lucuma.core.model.CloudExtinction
import cats.implicits.*
import Cat.*

class ConditionsCategoryTest extends FunSuite {
  private val oc = ObservingConditions(CloudExtinction.Preset.PointThree, IQ20, SB20, WV20)

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
    val cat = Cat(Eq(CloudExtinction.Preset.PointThree), Eq(IQ20))
    assert(cat.matches(oc))
    assert(!cat.matches(ObservingConditions(CloudExtinction.Preset.Zero, IQ20, SB20, WV20)))
    assert(!cat.matches(ObservingConditions(CloudExtinction.Preset.PointThree, IQ70, SB20, WV20)))
  }

  test("testAll") {
    val cat = Cat(Eq(CloudExtinction.Preset.PointThree), Eq(IQ20), Eq(SB20), Eq(WV20))
    assert(cat.matches(oc))
    assert(!cat.matches(ObservingConditions(CloudExtinction.Preset.PointThree, IQ20, SB50, WV20)))
    assert(!cat.matches(ObservingConditions(CloudExtinction.Preset.PointThree, IQ20, SB20, WV50)))
  }

  test("testUnspecified") {
    val cat = Cat()
    assert(cat.matches(oc))
    assert(cat.matches(ObservingConditions(CloudExtinction.Preset.PointThree, IQ20, SB50, WV20)))
    assert(cat.matches(ObservingConditions(CloudExtinction.Preset.PointThree, IQ20, SB20, WV50)))
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
    val cat = Le[SkyBackground](SB50)
    SkyBackground.values foreach { sb => assert(cat.canObserve(sb)) }
  }

  test("testCanObserveGe") {
    val cat = Ge[CloudExtinction.Preset](CloudExtinction.Preset.PointThree)
    List(CloudExtinction.Preset.PointThree, CloudExtinction.Preset.OnePointZero, CloudExtinction.Preset.ThreePointZero) foreach { cc => assert(cat.canObserve(cc)) }
    assert(!cat.canObserve(CloudExtinction.Preset.Zero))
  }

  test("testCanObserveAll") {
    val cat1 = Cat(Eq(CloudExtinction.Preset.Zero), Eq(IQ20), Le(SB50))
    val cat2 = Cat(Eq(CloudExtinction.Preset.Zero), Eq(IQ20), Ge(SB80))
    val cat3 = Cat(Ge(CloudExtinction.Preset.PointThree), Eq(IQ20))

    val oc = ObservingConditions(CloudExtinction.Preset.OnePointZero, IQ20, SB50, WVAny)

    assert(cat3.canObserve(oc))
    assert(!cat2.canObserve(oc))
    assert(cat1.canObserve(oc))

    val sp = SearchPath(List(cat1, cat2, cat3))
    assertEquals(List(cat3, cat1), sp(oc))
  }
}
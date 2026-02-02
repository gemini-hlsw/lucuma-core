// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.CloudCover.*
import edu.gemini.tac.qengine.p1.ImageQuality.*
import edu.gemini.tac.qengine.p1.SkyBackground.*
import edu.gemini.tac.qengine.p1.WaterVapor.*

import Cat.*
import munit.FunSuite

class ConditionsCategoryTest extends FunSuite {
  private val oc = ObservingConditions(CC70, IQ20, SB20, WV20)

  test("testEq") {
    val matches = CloudCover.values.map(x => Cat(ccSpec=Eq(x)).matches(oc))
    assertEquals(List(false, true, false, false), matches)
  }

  test("testLe") {
    val matches = CloudCover.values.map(x => Cat(ccSpec=Le(x)).matches(oc))
    assertEquals(List(false, true, true, true), matches)
  }

  test("testGe") {
    val matches = CloudCover.values.map(x => Cat(ccSpec=Ge(x)).matches(oc))
    assertEquals(List(true, true, false, false), matches)
  }

  test("testMultiple") {
    val cat = Cat(Eq(CC70), Eq(IQ20))
    assert(cat.matches(oc))
    assert(!cat.matches(ObservingConditions(CC50, IQ20, SB20, WV20)))
    assert(!cat.matches(ObservingConditions(CC70, IQ70, SB20, WV20)))
  }

  test("testAll") {
    val cat = Cat(Eq(CC70), Eq(IQ20), Eq(SB20), Eq(WV20))
    assert(cat.matches(oc))
    assert(!cat.matches(ObservingConditions(CC70, IQ20, SB50, WV20)))
    assert(!cat.matches(ObservingConditions(CC70, IQ20, SB20, WV50)))
  }

  test("testUnspecified") {
    val cat = Cat()
    assert(cat.matches(oc))
    assert(cat.matches(ObservingConditions(CC70, IQ20, SB50, WV20)))
    assert(cat.matches(ObservingConditions(CC70, IQ20, SB20, WV50)))
  }

  test("testCanObserveEq") {
    val cat0 = Eq[CloudCover](CC50)
    val cat1 = Eq[CloudCover](CC70)
    assert(cat0.canObserve(CC50))
    assert(cat0.canObserve(CC70))
    assert(!cat1.canObserve(CC50))
    assert(cat0.canObserve(CC70))
  }

  test("testCanObserveLe") {
    val cat = Le[SkyBackground](SB50)
    SkyBackground.values foreach { sb => assert(cat.canObserve(sb)) }
  }

  test("testCanObserveGe") {
    val cat = Ge[CloudCover](CC70)
    List(CC70, CC80, CCAny) foreach { cc => assert(cat.canObserve(cc)) }
    assert(!cat.canObserve(CC50))
  }

  test("testCanObserveAll") {
    val cat1 = Cat(Eq(CC50), Eq(IQ20), Le(SB50))
    val cat2 = Cat(Eq(CC50), Eq(IQ20), Ge(SB80))
    val cat3 = Cat(Ge(CC70), Eq(IQ20))

    val oc = ObservingConditions(CC80, IQ20, SB50)

    assert(cat3.canObserve(oc))
    assert(!cat2.canObserve(oc))
    assert(cat1.canObserve(oc))

    val sp = SearchPath(List(cat1, cat2, cat3))
    assertEquals(List(cat3, cat1), sp(oc))
  }
}
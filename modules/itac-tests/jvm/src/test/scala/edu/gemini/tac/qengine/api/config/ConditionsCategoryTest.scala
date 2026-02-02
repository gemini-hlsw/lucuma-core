// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1._
import edu.gemini.tac.qengine.p1.CloudCover._
import edu.gemini.tac.qengine.p1.ImageQuality._
import edu.gemini.tac.qengine.p1.SkyBackground._
import edu.gemini.tac.qengine.p1.WaterVapor._

import edu.gemini.tac.qengine.api.config.{ConditionsCategory => Cat}
import Cat._

import org.junit._
import Assert._

class ConditionsCategoryTest {
  private val oc = ObservingConditions(CC70, IQ20, SB20, WV20)

  @Test def testEq() = {
    val matches = CloudCover.values.map(x => Cat(ccSpec=Eq(x)).matches(oc))
    assertEquals(List(false, true, false, false), matches)
  }

  @Test def testLe() = {
    val matches = CloudCover.values.map(x => Cat(ccSpec=Le(x)).matches(oc))
    assertEquals(List(false, true, true, true), matches)
  }

  @Test def testGe() = {
    val matches = CloudCover.values.map(x => Cat(ccSpec=Ge(x)).matches(oc))
    assertEquals(List(true, true, false, false), matches)
  }

  @Test def testMultiple() = {
    val cat = Cat(Eq(CC70), Eq(IQ20))
    assertTrue(cat.matches(oc))
    assertFalse(cat.matches(ObservingConditions(CC50, IQ20, SB20, WV20)))
    assertFalse(cat.matches(ObservingConditions(CC70, IQ70, SB20, WV20)))
  }

  @Test def testAll() = {
    val cat = Cat(Eq(CC70), Eq(IQ20), Eq(SB20), Eq(WV20))
    assertTrue(cat.matches(oc))
    assertFalse(cat.matches(ObservingConditions(CC70, IQ20, SB50, WV20)))
    assertFalse(cat.matches(ObservingConditions(CC70, IQ20, SB20, WV50)))
  }

  @Test def testUnspecified() = {
    val cat = Cat()
    assertTrue(cat.matches(oc))
    assertTrue(cat.matches(ObservingConditions(CC70, IQ20, SB50, WV20)))
    assertTrue(cat.matches(ObservingConditions(CC70, IQ20, SB20, WV50)))
  }

  @Test def testCanObserveEq() = {
    val cat0 = Eq[CloudCover](CC50)
    val cat1 = Eq[CloudCover](CC70)
    assertTrue(cat0.canObserve(CC50))
    assertTrue(cat0.canObserve(CC70))
    assertFalse(cat1.canObserve(CC50))
    assertTrue(cat0.canObserve(CC70))
  }

  @Test def testCanObserveLe() = {
    val cat = Le[SkyBackground](SB50)
    SkyBackground.values foreach { sb => assertTrue(cat.canObserve(sb)) }
  }

  @Test def testCanObserveGe() = {
    val cat = Ge[CloudCover](CC70)
    List(CC70, CC80, CCAny) foreach { cc => assertTrue(cat.canObserve(cc)) }
    assertFalse(cat.canObserve(CC50))
  }

  @Test def testCanObserveAll() = {
    val cat1 = Cat(Eq(CC50), Eq(IQ20), Le(SB50))
    val cat2 = Cat(Eq(CC50), Eq(IQ20), Ge(SB80))
    val cat3 = Cat(Ge(CC70), Eq(IQ20))

    val oc = ObservingConditions(CC80, IQ20, SB50)

    assertTrue(cat3.canObserve(oc))
    assertFalse(cat2.canObserve(oc))
    assertTrue(cat1.canObserve(oc))

    val sp = SearchPath(List(cat1, cat2, cat3))
    assertEquals(List(cat3, cat1), sp(oc))
  }
}
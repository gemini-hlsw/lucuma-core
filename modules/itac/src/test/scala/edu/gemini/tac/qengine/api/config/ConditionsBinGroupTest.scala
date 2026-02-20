// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import cats.implicits.*
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Eq
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.IntCentiPercent
import munit.FunSuite

class ConditionsCategoryMapTest extends FunSuite {
  val cat0 = ConditionsCategory(Eq(CloudExtinction.Preset.Zero))
  val cat1 = ConditionsCategory(Eq(CloudExtinction.Preset.PointThree))
  val cat2 = ConditionsCategory(Eq(CloudExtinction.Preset.OnePointZero))
  val cat3 = ConditionsCategory(Eq(CloudExtinction.Preset.ThreePointZero))
  val bin0 = ConditionsBin(cat0, IntCentiPercent.unsafeFromPercent(10))
  val bin1 = ConditionsBin(cat1, IntCentiPercent.unsafeFromPercent(20))
  val bin2 = ConditionsBin(cat2, IntCentiPercent.unsafeFromPercent(30))
  val bin3 = ConditionsBin(cat3, IntCentiPercent.unsafeFromPercent(40))
  val oc0  = ConstraintSet(ImageQuality.Preset.PointOne, CloudExtinction.Preset.Zero,  SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)
  val oc1  = ConstraintSet(ImageQuality.Preset.PointOne, CloudExtinction.Preset.PointThree,  SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)
  val oc2  = ConstraintSet(ImageQuality.Preset.PointOne, CloudExtinction.Preset.OnePointZero,  SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)
  val oc3  = ConstraintSet(ImageQuality.Preset.PointOne, CloudExtinction.Preset.ThreePointZero, SkyBackground.Darkest, WaterVapor.VeryDry, ElevationRange.ByAirMass.Default)

  val grp = ConditionsCategoryMap.of(List(bin0, bin1, bin2, bin3))

  def validatePath[A](grp: ConditionsCategoryMap[A]) = {
    assertEquals(List(cat0), grp.searchPath(oc0))
    assertEquals(List(cat1, cat0), grp.searchPath(oc1))
    assertEquals(List(cat2, cat1, cat0), grp.searchPath(oc2))
    assertEquals(List(cat3, cat2, cat1, cat0), grp.searchPath(oc3))
  }

  test("testCreation") {
    assertEquals(4, grp.bins.size)
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp.bins(cat0))
    assertEquals(IntCentiPercent.unsafeFromPercent(20), grp.bins(cat1))
    assertEquals(IntCentiPercent.unsafeFromPercent(30), grp.bins(cat2))
    assertEquals(IntCentiPercent.unsafeFromPercent(40), grp.bins(cat3))
    validatePath(grp)
  }

  test("testMap") {
    // map percents to integer values
    val grp2 = grp.map(_.toPercent)
    assertEquals(4, grp2.bins.size)
    assertEquals(10.0, grp2.bins(cat0).toDouble)
    assertEquals(20.0, grp2.bins(cat1).toDouble)
    assertEquals(30.0, grp2.bins(cat2).toDouble)
    assertEquals(40.0, grp2.bins(cat3).toDouble)
    validatePath(grp2)
  }

  test("testLookup") {
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp(cat0))
    assertEquals(IntCentiPercent.unsafeFromPercent(20), grp(cat1))
    assertEquals(IntCentiPercent.unsafeFromPercent(30), grp(cat2))
    assertEquals(IntCentiPercent.unsafeFromPercent(40), grp(cat3))
  }

  test("testSimpleUpdated") {
    val grp2 = grp.updated(cat2, IntCentiPercent.unsafeFromPercent(42))
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp2(cat0))
    assertEquals(IntCentiPercent.unsafeFromPercent(20), grp2(cat1))
    assertEquals(IntCentiPercent.unsafeFromPercent(42), grp2(cat2))
    assertEquals(IntCentiPercent.unsafeFromPercent(40), grp2(cat3))
    validatePath(grp2)
  }

  test("testSimpleUpdatedOC") {
    val grp2 = grp.updated(oc2, IntCentiPercent.unsafeFromPercent(42))
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp2(cat0))
    assertEquals(IntCentiPercent.unsafeFromPercent(20), grp2(cat1))
    assertEquals(IntCentiPercent.unsafeFromPercent(42), grp2(cat2))
    assertEquals(IntCentiPercent.unsafeFromPercent(40), grp2(cat3))
    validatePath(grp2)
  }

  test("testMultipleUpdated") {
    val grp2 = grp.updated(List((cat1, IntCentiPercent.unsafeFromPercent(11)), (cat2, IntCentiPercent.unsafeFromPercent(22))))
    assertEquals(IntCentiPercent.unsafeFromPercent(10), grp2(cat0))
    assertEquals(IntCentiPercent.unsafeFromPercent(11), grp2(cat1))
    assertEquals(IntCentiPercent.unsafeFromPercent(22), grp2(cat2))
    assertEquals(IntCentiPercent.unsafeFromPercent(40), grp2(cat3))
    validatePath(grp2)
  }

  test("testUndefinedSimpleUpdated") {
    try {
      grp.updated(ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.PointOne)), IntCentiPercent.unsafeFromPercent(99))
      fail
    } catch {
      case _: IllegalArgumentException => // ok
    }
  }

  test("testUndefinedMuliptleUpdated") {
    val cat4 = ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.PointOne))
    try {
      grp.updated(List((cat0, IntCentiPercent.unsafeFromPercent(1)), (cat4, IntCentiPercent.unsafeFromPercent(99))))
      fail
    } catch {
      case _: IllegalArgumentException => // ok
    }

  }

  /*
  test("testFunctionUpdated") {
    val grp2 = grp.updated(oc2, vals => Some(vals.map(perc => IntCentiPercent(perc.value * 2)))).get

    assertEquals(IntCentiPercent(20), grp2(cat0))
    assertEquals(IntCentiPercent(40), grp2(cat1))
    assertEquals(IntCentiPercent(60), grp2(cat2))
    assertEquals(IntCentiPercent(40), grp2(cat3))
  }
  */
}
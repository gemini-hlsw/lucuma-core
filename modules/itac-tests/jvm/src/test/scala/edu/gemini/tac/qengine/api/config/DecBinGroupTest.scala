// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import org.junit._
import Assert._
import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.{Percent, Angle}

class DecBinGroupTest {

  private def f(decRange: DecRange): Option[Percent] = {
    decRange.startDeg match {
      case neg if neg < 0 => None
      case pos => Some(Percent(decRange.startDeg))
    }
  }

  @Test def testGen10deg() = {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(None, grp.get(new Angle(-10, Angle.Deg)))
    assertEquals(Percent(0), grp.get(Angle.angleDeg0).get.binValue)
    assertEquals(Percent(0), grp.get(new Angle(9.9, Angle.Deg)).get.binValue)
    assertEquals(Percent(80), grp.get(new Angle(80, Angle.Deg)).get.binValue)
    assertEquals(Percent(80), grp.get(new Angle(90, Angle.Deg)).get.binValue)
  }

  @Test def testGen20deg() = {
    val grp = DecBinGroup.gen20DegBins(f)
    assertEquals(None, grp.get(new Angle(-10, Angle.Deg)))
    assertEquals(None, grp.get(Angle.angleDeg0))
    assertEquals(Percent(10), grp.get(new Angle(10, Angle.Deg)).get.binValue)
    assertEquals(Percent(10), grp.get(new Angle(19.9, Angle.Deg)).get.binValue)
    assertEquals(Percent(70), grp.get(new Angle(70, Angle.Deg)).get.binValue)
    assertEquals(Percent(70), grp.get(new Angle(90, Angle.Deg)).get.binValue)
  }

  @Test def testBadBinSize() = {
    try {
      DecBinGroup.gen(17)(f)
      fail
    } catch {
      case ex: IllegalArgumentException => // ok
    }
  }

  @Test def testUpdatedFunSome() = {
    val grp = DecBinGroup.gen10DegBins(f)
    val grp2 = grp.updated(Angle.angleDeg0, p => Some(Percent(p.value + 1))).get
    assertEquals(Percent(1), grp2.get(Angle.angleDeg0).get.binValue)
  }

  @Test def testUpdatedFunNone() = {
    val grp = DecBinGroup.gen10DegBins(f)
    val opt = grp.updated(new Angle(-10, Angle.Deg), p => Some(Percent(p.value + 1)))
    assertEquals(None, opt)
  }

  @Test def testUpdatedSome() = {
    val grp = DecBinGroup.gen10DegBins(f)
    val grp2 = grp.updated(Angle.angleDeg0, Percent(42)).get
    assertEquals(Percent(42), grp2.get(Angle.angleDeg0).get.binValue)
  }

  @Test def testUpdatedNone() = {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(None, grp.updated(new Angle(-10, Angle.Deg), Percent(42)))
  }

  @Test def testIndexOf() = {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(-1, grp.indexOf(new Angle(-10, Angle.Deg)))
    assertEquals( 0, grp.indexOf(Angle.angleDeg0))
    assertEquals( 8, grp.indexOf(new Angle( 80, Angle.Deg)))
    assertEquals( 8, grp.indexOf(new Angle( 90, Angle.Deg)))
  }

  @Test def testApplyTarget() = {
    val grp = DecBinGroup.gen10DegBins(f)
    assertEquals(Percent( 0), grp.get(Target(new Angle(12, Angle.Hr), Angle.angleDeg0)).get.binValue)
    assertEquals(Percent(70), grp.get(Target(new Angle(12, Angle.Hr), new Angle(70, Angle.Deg))).get.binValue)
    assertEquals(None, grp.get(Target(new Angle(12, Angle.Hr), new Angle(-90, Angle.Deg))))
  }

  @Test def testMap() = {
    val grp = DecBinGroup.gen10DegBins(f)
    val grpInt = grp.map(_.value)
    assertEquals(None, grpInt.get(new Angle(-1, Angle.Deg)))
    assertEquals(   0, grpInt.get(Angle.angleDeg0).get.binValue.doubleValue, Double.MinPositiveValue)
    assertEquals(  10, grpInt.get(new Angle(15, Angle.Deg)).get.binValue.doubleValue, Double.MinPositiveValue)
  }

  @Test def testGenFromBinValues() = {
    val percs = List(Percent(0), Percent(25), Percent(100), Percent(50))
    val grp = DecBinGroup(percs)

    val expected = List(
      DecBin(DecRange(-90, -45), Percent(  0)),
      DecBin(DecRange(-45,   0), Percent( 25)),
      DecBin(DecRange(  0,  45), Percent(100)),
      DecBin(DecRange( 45,  90).inclusive, Percent(50))
    )

    assertEquals(expected, grp.bins)
  }
}
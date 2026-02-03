// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.Angle
import edu.gemini.tac.qengine.util.Time
import munit.FunSuite

import scala.annotation.unused

class RaBinGroupTest extends FunSuite {

  // The function just generates a Time amount in minutes equal to the Angle.
  // So, a 30 min Angle yields 30 minutes of time.  This makes it easy to test.
  // A real function would compute the number of minutes that the RA is
  // visible at night and return that amount of time.
  private def f(a: Angle, @unused sizeMin: Int): Time = Time.minutes(a.mag)

  test("testGenerate1Hr") {
    val g1Hr = RaBinGroup.gen1HrBins(f)
    assertEquals(24, g1Hr.bins.length)

    val times = for (i <- 0 until 24) yield Time.minutes(i * 60 + 30)
    assertEquals(times, g1Hr.bins)
  }

  test("testGenerate2Hr") {
    val g2Hr = RaBinGroup.gen2HrBins(f)
    assertEquals(12, g2Hr.bins.length)

    val times = for (i <- 0 until 12) yield Time.minutes(i * 120 + 60)
    assertEquals(times, g2Hr.bins)
  }

  private def validateLookup(lookup: (RaBinGroup[Time], Int) => Time) = {
    val g = RaBinGroup.gen1HrBins(f)
    assertEquals(Time.minutes(30), lookup(g, 0))
    assertEquals(Time.minutes(30), lookup(g, 15))
    assertEquals(Time.minutes(30), lookup(g, 59))
    assertEquals(Time.minutes(90), lookup(g, 60))
    assertEquals(Time.minutes(1410), lookup(g, 24*60-1))

    // wrap around
    assertEquals(Time.minutes(30), lookup(g, 24*60))
  }

  test("testLookupMin") {
    validateLookup((g, m) => g(m))
  }

  test("testLookupAngle") {
    validateLookup((g, m) => g(new Angle(m, Angle.Min)))
  }

  test("testLookupTarget") {
    validateLookup((g, m) => g(Target(new Angle(m, Angle.Min), Angle.angleDeg0)))
  }

  test("testMap") {
    val gTime = RaBinGroup.gen1HrBins(f)

    // Map the Time objects to Ints with the corresponding value in minutes
    val gInt  = gTime.map(_.value.toInt)
    assertEquals(24, gInt.bins.length)

    assertEquals(30, gInt(0))
  }

  test("testUpdated") {
    val ra0 = new Angle( 0, Angle.Min)
    val ra1 = new Angle(60, Angle.Min)
    val min1 = Time.minutes(1)
    val bg = RaBinGroup.gen1HrBins(f).updated(ra0, Time.Zero).updated(ra1, min1)
    assertEquals(Time.Zero, bg(ra0))
    assertEquals(min1, bg(ra1))
  }

  // Need a fake function that sometimes returns None to simulate what
  // happens when the value cannot be updated.
  private val doubleEveryOther = (t: Time) => t.value.toInt match {
      case n if (n-30 % 120) == 0 => Some(t + t)
      case _ => None
    }

  test("testUpdatedFunSome") {
    val bg0 = RaBinGroup.gen1HrBins(f)

    val ra0 = new Angle(0, Angle.Min)
    bg0.updated(ra0, doubleEveryOther) match {
      case Some(bg1) => assertEquals(bg0(ra0).value.toInt * 2, bg1(ra0).value.toInt)
      case _ => fail
    }
  }

  test("testUpdatedFunNone") {
  val bg0 = RaBinGroup.gen1HrBins(f)

    val ra1 = new Angle(60, Angle.Min)
    bg0.updated(ra1, doubleEveryOther) match {
      case None => // ok
      case _ => fail
    }
  }
}
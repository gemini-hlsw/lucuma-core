// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.util.TimeSpan
import munit.FunSuite
import lucuma.core.model.IntCentiPercentUnbounded

class SiteSemesterConfigTest extends FunSuite {
  // these aren't really relevant for the test cases, but required to
  // construct the SiteSemesterConfig
  val site     = Site.GN
  val semester = new Semester(YearInt.unsafeFrom(2011), Half.A)

  test("testPassSingleDecBinPercentageRequirement") {
    val ra  = RightAscensionMap(List(TimeSpan.fromHoursBounded(100.0)))
    val dec = DeclinationMap(List(IntCentiPercentUnbounded.unsafeFromPercent(100)))
    new SiteSemesterConfig(site, semester, ra, dec, List.empty)
    ()
  }

  test("testFailSingleDecBinPercentageRequirement") {
    val ra  = RightAscensionMap(List(TimeSpan.fromHoursBounded(100.0)))
    val dec = DeclinationMap(List(IntCentiPercentUnbounded.unsafeFromPercent(99)))
    try {
      new SiteSemesterConfig(site, semester, ra, dec, List.empty)
      ()
    } catch {
      case ex: IllegalArgumentException => // expected
    }    
  }

  test("testPassMultiDecBinPercentageRequirement") {
    val ra  = RightAscensionMap(List(TimeSpan.fromHoursBounded(100.0)))
    val dec = DeclinationMap(List(IntCentiPercentUnbounded.unsafeFromPercent(10), IntCentiPercentUnbounded.unsafeFromPercent(100), IntCentiPercentUnbounded.unsafeFromPercent(0)))
    new SiteSemesterConfig(site, semester, ra, dec, List.empty)
    ()
  }

  test("testFailMultiDecBinPercentageRequirement") {
    val ra  = RightAscensionMap(List(TimeSpan.fromHoursBounded(100.0)))
    val dec = DeclinationMap(List(IntCentiPercentUnbounded.unsafeFromPercent(10), IntCentiPercentUnbounded.unsafeFromPercent(99), IntCentiPercentUnbounded.unsafeFromPercent(0)))
    try {
      new SiteSemesterConfig(site, semester, ra, dec, List.empty)
      ()
    } catch {
      case ex: IllegalArgumentException => // expected
    }
  }
}
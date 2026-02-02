// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.spModel.core.{Semester, Site}
import edu.gemini.tac.qengine.util.{Percent, Time}

import org.junit._

class SiteSemesterConfigTest {
  // these aren't really relevant for the test cases, but required to
  // construct the SiteSemesterConfig
  val site     = Site.GN
  val semester = new Semester(2011, Semester.Half.A)

  @Test def testPassSingleDecBinPercentageRequirement() = {
    val ra  = RaBinGroup(List(Time.hours(100.0)))
    val dec = DecBinGroup(List(Percent(100)))
    new SiteSemesterConfig(site, semester, ra, dec, List.empty)
  }

  @Test def testFailSingleDecBinPercentageRequirement() = {
    val ra  = RaBinGroup(List(Time.hours(100.0)))
    val dec = DecBinGroup(List(Percent(99)))
    try {
      new SiteSemesterConfig(site, semester, ra, dec, List.empty)
    } catch {
      case ex: IllegalArgumentException => // expected
    }
  }

  @Test def testPassMultiDecBinPercentageRequirement() = {
    val ra  = RaBinGroup(List(Time.hours(100.0)))
    val dec = DecBinGroup(List(Percent(10), Percent(100), Percent(0)))
    new SiteSemesterConfig(site, semester, ra, dec, List.empty)
  }

  @Test def testFailMultiDecBinPercentageRequirement() = {
    val ra  = RaBinGroup(List(Time.hours(100.0)))
    val dec = DecBinGroup(List(Percent(10), Percent(99), Percent(0)))
    try {
      new SiteSemesterConfig(site, semester, ra, dec, List.empty)
    } catch {
      case ex: IllegalArgumentException => // expected
    }
  }
}
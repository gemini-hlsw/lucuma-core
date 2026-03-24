// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.enums.Half.*
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.model.TwilightBoundedNight
import munit.FunSuite

class SemesterTest extends FunSuite{

  test("testNightIterator") {

    val tpe      = TwilightType.Nautical
    val site     = Site.GS
    val semester = Semester(YearInt.unsafeFrom(2010), B)
    val nights   = NightIterator.bounded(tpe, site, semester).toList

    val firstExpected = TwilightBoundedNight.fromTwilightTypeAndSiteAndInstantUnsafe(tpe, site, semester.start.atSite(site).toInstant())
    val lastExpected  = TwilightBoundedNight.fromTwilightTypeAndSiteAndInstantUnsafe(tpe, site, semester.end.atSite(site).toInstant())

    assertEquals(nights.head.interval, firstExpected.interval)
    assertEquals(nights.last.interval, lastExpected.interval)

  }

}

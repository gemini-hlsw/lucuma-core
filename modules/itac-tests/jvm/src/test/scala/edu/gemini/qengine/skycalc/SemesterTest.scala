// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import org.junit._
import Assert._

import edu.gemini.spModel.core.Semester
import edu.gemini.spModel.core.Site

import Semester.Half._
import java.util.{GregorianCalendar, Calendar}
import edu.gemini.skycalc.{Night, TwilightBoundedNight, TwilightBoundType}

class SemesterTest {

  private def mkCal: Calendar = {
    val cal = new GregorianCalendar(Site.GS.timezone)
    cal.set(Calendar.MILLISECOND, 0)
    cal
  }

  private def verifyNight(expected: Night, actual: Night) {
    assertEquals(expected.getStartTime, actual.getStartTime)
    assertEquals(expected.getEndTime, actual.getEndTime)
    assertEquals(expected.getSite, actual.getSite)
  }

  @Test def testNightIterator() {
    val sem = new Semester(2010, B)

    val nautical = TwilightBoundType.NAUTICAL
    val cp       = Site.GS
    val it       = new NightIterator(Site.GS, sem)

    assertTrue(it.hasNext)
    val firstNight = it.next;

    val cStart = mkCal
    cStart.set(2010, B.getStartMonth, 1, 14, 0, 0)
    cStart.add(Calendar.DAY_OF_MONTH, -1)

    val firstExpected = new TwilightBoundedNight(nautical, cStart.getTimeInMillis, cp)
    verifyNight(firstExpected, firstNight)

    var lastNight = firstNight
    while (it.hasNext) {
      lastNight = it.next
    }

    val endTime = sem.getEndDate(Site.GS).getTime
    val cEnd    = mkCal
    cEnd.setTimeInMillis(endTime)
    cEnd.add(Calendar.DAY_OF_MONTH, -1)

    val lastExpected = new TwilightBoundedNight(nautical, cEnd.getTimeInMillis, cp)
    verifyNight(lastExpected, lastNight)
  }

}

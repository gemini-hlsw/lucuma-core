// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import java.util.{Date, Calendar, GregorianCalendar}
import lucuma.core.enums.Site
import java.util.TimeZone
import munit.FunSuite
import java.time.Duration
import lucuma.core.model.Night
import lucuma.core.model.TwilightBoundedNight
import java.time.Instant
import lucuma.core.enums.TwilightType

class NightIteratorTest extends FunSuite {
  val site = Site.GS

  def mkCal(year: Int, month: Int, day: Int, hr: Int): Calendar = {
    val cal = new GregorianCalendar(TimeZone.getTimeZone(site.timezone))
    cal.set(year, month, day, hr, 0, 0)
    cal.set(Calendar.MILLISECOND, 0)
    cal
  }

  test("testEndsBeforeBegins") {
    val cal0 = mkCal(2011, Calendar.JANUARY, 2, 16) // Jan 2, 2 PM
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 16) // Jan 1, 4 PM

    val ni = NightIterator.bounded(TwilightType.Nautical, site, cal0.getTime, cal1.getTime)
    assertEquals(0, ni.toList.size)
  }

  test("testNoNightTime") {
    val cal0 = mkCal(2011, Calendar.JANUARY, 1, 14) // 2 PM
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 16) // 4 PM

    val ni = NightIterator.bounded(TwilightType.Nautical, site, cal0.getTime, cal1.getTime)
    assertEquals(0, ni.toList.size)
  }

  test("testTrimOne") {
    val cal0 = mkCal(2011, Calendar.JANUARY, 1, 23) // 11 PM, Jan 1
    val cal1 = mkCal(2011, Calendar.JANUARY, 2, 0)  // 12 AM, Jan 2

    val ni  = NightIterator.bounded(TwilightType.Nautical, site, cal0.getTime, cal1.getTime)
    val lst = ni.toList
    assertEquals(1, lst.size)

    // Just the one hour that is in the interval.
    val night = lst.head
    assertEquals(Duration.ofHours(1), night.duration)
  }

  private def wholeNight(d: Date): Night =
    TwilightBoundedNight.fromTwilightTypeAndSiteAndInstant(TwilightType.Nautical, site, Instant.ofEpochMilli(d.getTime())).get

  test("testTrimTwo") {
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 23) // 11 PM, Jan 1
    val cal2 = mkCal(2011, Calendar.JANUARY, 2, 23) // 11 PM, Jan 2

    val ni  = NightIterator.bounded(TwilightType.Nautical, site, cal1.getTime, cal2.getTime)
    val lst = ni.toList
    assertEquals(2, lst.size)

    val jan1 = wholeNight(cal1.getTime) // Night of Jan 1
    val jan2 = wholeNight(cal2.getTime) // Night of Jan 2

    // Just the one hour that is in the interval.
    val n1 = lst(0)
    val n2 = lst(1)

    assertEquals(Instant.ofEpochMilli(cal1.getTimeInMillis), n1.start) // Starts at 11 PM Jan 1
    assertEquals(jan1.end, n1.end)        // Ends at twilight dawn Jan 2
    assertEquals(jan2.start, n2.start)    // Starts at dusk Jan 2
    assertEquals(Instant.ofEpochMilli(cal2.getTimeInMillis), n2.end)   // Ends at 11 PM, Jan 2
  }

  test("testTrimDaylight") {
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 14) // 2 PM, Jan 1
    val cal2 = mkCal(2011, Calendar.JANUARY, 2, 14) // 2 PM, Jan 2

    val ni  = NightIterator.bounded(TwilightType.Nautical, site, cal1.getTime, cal2.getTime)
    val lst = ni.toList
    assertEquals(1, lst.size)

    val jan1 = wholeNight(cal1.getTime) // Night of Jan 1

    // Just the one hour that is in the interval.
    val n = lst(0)

    assertEquals(jan1.start, n.start)
    assertEquals(jan1.end, n.end)
  }

  test("testTrimSecondDayDaylight") {
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 14) // 2 PM, Jan 1
    val cal2 = mkCal(2011, Calendar.JANUARY, 2, 14) // 2 PM, Jan 2
    val cal3 = mkCal(2011, Calendar.JANUARY, 3, 14) // 2 PM, Jan 2

    val ni  = NightIterator.bounded(TwilightType.Nautical, site, cal1.getTime, cal3.getTime)
    val lst = ni.toList
    assertEquals(2, lst.size)

    val jan1 = wholeNight(cal1.getTime) // Night of Jan 1
    val jan2 = wholeNight(cal2.getTime) // Night of Jan 1

    // Just the one hour that is in the interval.
    val n1 = lst(0)
    val n2 = lst(1)

    assertEquals(jan1.start, n1.start)
    assertEquals(jan1.end, n1.end)
    assertEquals(jan2.start, n2.start)
    assertEquals(jan2.end, n2.end)
  }
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import org.junit._
import Assert._
import edu.gemini.spModel.core.Site
import java.util.{Date, Calendar, GregorianCalendar}
import edu.gemini.skycalc.{Night, TwilightBoundType, TwilightBoundedNight}

class NightIteratorTest {
  val site = Site.GS

  def mkCal(year: Int, month: Int, day: Int, hr: Int): Calendar = {
    val cal = new GregorianCalendar(site.timezone)
    cal.set(year, month, day, hr, 0, 0)
    cal.set(Calendar.MILLISECOND, 0)
    cal
  }

  @Test def testEndsBeforeBegins() {
    val cal0 = mkCal(2011, Calendar.JANUARY, 2, 16) // Jan 2, 2 PM
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 16) // Jan 1, 4 PM

    val ni = new NightIterator(site, cal0.getTime, cal1.getTime)
    assertEquals(0, ni.toList.size)
  }

  @Test def testNoNightTime() {
    val cal0 = mkCal(2011, Calendar.JANUARY, 1, 14) // 2 PM
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 16) // 4 PM

    val ni = new NightIterator(site, cal0.getTime, cal1.getTime)
    assertEquals(0, ni.toList.size)
  }

  @Test def testTrimOne() {
    val cal0 = mkCal(2011, Calendar.JANUARY, 1, 23) // 11 PM, Jan 1
    val cal1 = mkCal(2011, Calendar.JANUARY, 2, 0)  // 12 AM, Jan 2

    val ni  = new NightIterator(site, cal0.getTime, cal1.getTime)
    val lst = ni.toList
    assertEquals(1, lst.size)

    // Just the one hour that is in the interval.
    val night = lst.get(0)
    assertEquals(60 * 60 * 1000, night.getTotalTime)
  }

  private def wholeNight(d: Date): Night =
    new TwilightBoundedNight(TwilightBoundType.NAUTICAL, d.getTime, site)

  @Test def testTrimTwo() {
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 23) // 11 PM, Jan 1
    val cal2 = mkCal(2011, Calendar.JANUARY, 2, 23) // 11 PM, Jan 2

    val ni  = new NightIterator(site, cal1.getTime, cal2.getTime)
    val lst = ni.toList
    assertEquals(2, lst.size)

    val jan1 = wholeNight(cal1.getTime) // Night of Jan 1
    val jan2 = wholeNight(cal2.getTime) // Night of Jan 2

    // Just the one hour that is in the interval.
    val n1 = lst.get(0)
    val n2 = lst.get(1)

    assertEquals(cal1.getTimeInMillis, n1.getStartTime) // Starts at 11 PM Jan 1
    assertEquals(jan1.getEndTime, n1.getEndTime)        // Ends at twilight dawn Jan 2
    assertEquals(jan2.getStartTime, n2.getStartTime)    // Starts at dusk Jan 2
    assertEquals(cal2.getTimeInMillis, n2.getEndTime)   // Ends at 11 PM, Jan 2
  }

  @Test def testTrimDaylight() {
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 14) // 2 PM, Jan 1
    val cal2 = mkCal(2011, Calendar.JANUARY, 2, 14) // 2 PM, Jan 2

    val ni  = new NightIterator(site, cal1.getTime, cal2.getTime)
    val lst = ni.toList
    assertEquals(1, lst.size)

    val jan1 = wholeNight(cal1.getTime) // Night of Jan 1

    // Just the one hour that is in the interval.
    val n = lst.get(0)

    assertEquals(jan1.getStartTime, n.getStartTime)
    assertEquals(jan1.getEndTime, n.getEndTime)
  }

  @Test def testTrimSecondDayDaylight() {
    val cal1 = mkCal(2011, Calendar.JANUARY, 1, 14) // 2 PM, Jan 1
    val cal2 = mkCal(2011, Calendar.JANUARY, 2, 14) // 2 PM, Jan 2
    val cal3 = mkCal(2011, Calendar.JANUARY, 3, 14) // 2 PM, Jan 2

    val ni  = new NightIterator(site, cal1.getTime, cal3.getTime)
    val lst = ni.toList
    assertEquals(2, lst.size)

    val jan1 = wholeNight(cal1.getTime) // Night of Jan 1
    val jan2 = wholeNight(cal2.getTime) // Night of Jan 1

    // Just the one hour that is in the interval.
    val n1 = lst.get(0)
    val n2 = lst.get(1)

    assertEquals(jan1.getStartTime, n1.getStartTime)
    assertEquals(jan1.getEndTime, n1.getEndTime)
    assertEquals(jan2.getStartTime, n2.getStartTime)
    assertEquals(jan2.getEndTime, n2.getEndTime)
  }
}

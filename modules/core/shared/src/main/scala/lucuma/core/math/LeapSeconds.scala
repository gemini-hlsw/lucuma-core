// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.syntax.option.*

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

object LeapSeconds {

  // List of UTC dates (at midnight UTC) when a positive leap second was inserted at 23:59:60 UTC.
  // Data from IANA: https://data.iana.org/time-zones/data/leap-seconds.list
  // NOTE: This list must be kept up to date as new leap seconds are announced.
  // Leap seconds will be abandoned by or before 2035, so this list will not grow indefinitely.
  // Note that 10 secs were added on 1/1/1972 at midnight to align UTC with TAI
  // See https://en.wikipedia.org/wiki/Leap_second
  private val LeapSecondDatesUtc: List[(LocalDate, Int)] = List(
    (LocalDate.of(1971, 12, 31), 10),
    (LocalDate.of(1972, 6, 30), 1),
    (LocalDate.of(1972, 12, 31), 1),
    (LocalDate.of(1973, 12, 31), 1),
    (LocalDate.of(1974, 12, 31), 1),
    (LocalDate.of(1975, 12, 31), 1),
    (LocalDate.of(1976, 12, 31), 1),
    (LocalDate.of(1977, 12, 31), 1),
    (LocalDate.of(1978, 12, 31), 1),
    (LocalDate.of(1979, 12, 31), 1),
    (LocalDate.of(1981, 6, 30), 1),
    (LocalDate.of(1982, 6, 30), 1),
    (LocalDate.of(1983, 6, 30), 1),
    (LocalDate.of(1985, 6, 30), 1),
    (LocalDate.of(1987, 12, 31), 1),
    (LocalDate.of(1989, 12, 31), 1),
    (LocalDate.of(1990, 12, 31), 1),
    (LocalDate.of(1992, 6, 30), 1),
    (LocalDate.of(1993, 6, 30), 1),
    (LocalDate.of(1994, 6, 30), 1),
    (LocalDate.of(1995, 12, 31), 1),
    (LocalDate.of(1997, 6, 30), 1),
    (LocalDate.of(1998, 12, 31), 1),
    (LocalDate.of(2005, 12, 31), 1),
    (LocalDate.of(2008, 12, 31), 1),
    (LocalDate.of(2012, 6, 30), 1),
    (LocalDate.of(2015, 6, 30), 1),
    (LocalDate.of(2016, 12, 31), 1)
  )

  // Build a list of (epochSecond, accumulatedLeapSeconds) for convenience when adding leap seconds to UTC.
  private val AddLeapSecondBoundaries: List[(Long, Int)] =
    LeapSecondDatesUtc
      .map: (date, diff) =>
        (date.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant.getEpochSecond, diff)
      .scan((Long.MinValue, 0)){ case ((_, accum), (epochSeconds, diff)) =>
        (epochSeconds, accum + diff)
      }

  /** The maximum number of leap seconds so far. */
  val Max: Int = AddLeapSecondBoundaries.last._2

  // Build a list of (epochSecond, accumulatedLeapSeconds) for convenience when removing leap seconds from TAI.
  private val RemoveLeapSecondBoundaries: List[(Long, Int)] =
    AddLeapSecondBoundaries.map: (epochSeconds, accum) =>
      (epochSeconds + accum, accum)

  // Take advantage of the sorted list to short circuit the count.
  private def sortedCountBefore(list: List[(Long, Int)], i: Long): Int = 
    val overIndex: Option[Int] = list.indexWhere(_._1 >= i).some.filter(_ >= 0)
    overIndex.map(idx => list(idx - 1)._2).getOrElse(Max)

  /** Returns the number of leap seconds that have occurred *strictly before* the given Instant. */
  def before(instant: Instant): Int = {
    sortedCountBefore(AddLeapSecondBoundaries, instant.getEpochSecond)
  }

  /** Returns the number of leap seconds that are included in the given TaiInstant. */
  def includedIn(taiInstant: TaiInstant): Int = {
    sortedCountBefore(RemoveLeapSecondBoundaries, taiInstant.value.getEpochSecond)
  }
}
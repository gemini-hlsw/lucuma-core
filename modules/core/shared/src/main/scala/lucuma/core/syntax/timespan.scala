// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import lucuma.core.util.TimeSpan

trait timespan:
  extension (inline i: Int) {
    inline def microsecondTimeSpan: TimeSpan =
      inline if (i >= 0) TimeSpan.fromMicroseconds(i).get
      else scala.compiletime.error("TimeSpan must be non-negative.")

    inline def µsTimeSpan: TimeSpan =
      microsecondTimeSpan

    inline def millisecondTimeSpan: TimeSpan =
      inline if (i >= 0) TimeSpan.fromMilliseconds(BigDecimal(i)).get
      else scala.compiletime.error("TimeSpan must be non-negative.")

    inline def msTimeSpan: TimeSpan =
      millisecondTimeSpan

    inline def secondTimeSpan: TimeSpan =
      inline if (i >= 0) TimeSpan.fromSeconds(BigDecimal(i)).get
      else scala.compiletime.error("TimeSpan must be non-negative.")

    inline def secTimeSpan: TimeSpan =
      secondTimeSpan

    inline def minuteTimeSpan: TimeSpan =
      inline if (i >= 0) TimeSpan.fromMinutes(BigDecimal(i)).get
      else scala.compiletime.error("TimeSpan must be non-negative.")

    inline def minTimeSpan: TimeSpan =
      minuteTimeSpan

    inline def hourTimeSpan: TimeSpan =
      inline if (i >= 0) TimeSpan.fromHours(BigDecimal(i)).get
      else scala.compiletime.error("TimeSpan must be non-negative.")

    inline def hrTimeSpan: TimeSpan =
      hourTimeSpan

  }

  extension (inline l: Long) {

    inline def microsecondTimeSpan: TimeSpan =
      inline if (l >= 0) TimeSpan.fromMicroseconds(l).get
      else scala.compiletime.error("TimeSpan must be non-negative.")

    inline def µsTimeSpan: TimeSpan =
      microsecondTimeSpan

    inline def millisecondTimeSpan: TimeSpan =
      inline if (l >= 0 && l <= Long.MaxValue / 1_000L) TimeSpan.fromMilliseconds(BigDecimal(l)).get
      else scala.compiletime.error("Out of range for TimeSpan in milliseconds.")

    inline def msTimeSpan: TimeSpan =
      millisecondTimeSpan

    inline def secondTimeSpan: TimeSpan =
      inline if (l >= 0 && l <= Long.MaxValue / 1_000_000L) TimeSpan.fromSeconds(BigDecimal(l)).get
      else scala.compiletime.error("Out of range for TimeSpan in seconds.")

    inline def secTimeSpan: TimeSpan =
      secondTimeSpan

    inline def minuteTimeSpan: TimeSpan =
      inline if (l >= 0 && l <= Long.MaxValue / 60_000_000L) TimeSpan.fromMinutes(BigDecimal(l)).get
      else scala.compiletime.error("Out of range for TimeSpan in minutes.")

    inline def minTimeSpan: TimeSpan =
      minuteTimeSpan

    inline def hourTimeSpan: TimeSpan =
      inline if (l >= 0 && l <= Long.MaxValue / 3_600_000_000L) TimeSpan.fromHours(BigDecimal(l)).get
      else scala.compiletime.error("Out of range for TimeSpan in hours.")

    inline def hrTimeSpan: TimeSpan =
      hourTimeSpan
  }

object timespan extends timespan
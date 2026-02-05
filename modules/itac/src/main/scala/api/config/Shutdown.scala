// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import cats.syntax.all.*
import lucuma.core.enums.Site

import java.text.SimpleDateFormat
import java.time.ZonedDateTime
import java.util.TimeZone
/**
 * Defines a shutdown period.
 */
case class Shutdown(site: Site, start: ZonedDateTime, end: ZonedDateTime) extends Ordered[Shutdown] {
  require(start.isBefore(end))

  def compare(that: Shutdown): Int = {
    def compareStartAndEndDates = {
      def compareDates(f: Shutdown => ZonedDateTime) = f(Shutdown.this).compareTo(f(that))
      val res = compareDates(_.start)
      if (res == 0) compareDates(_.end) else res
    }

    val res = site.compare(that.site)
    if (res == 0) compareStartAndEndDates else res
  }

  def toDateString: String = {
    val df = new SimpleDateFormat("MMM d")
    df.setTimeZone(TimeZone.getTimeZone(site.timezone))

    "%s - %s".format(df.format(start), df.format(end))
  }

  override def toString: String =
    "%s %s".format(site.shortName, toDateString)
}
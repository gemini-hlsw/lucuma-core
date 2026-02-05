// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

/**
 * Contains a time amount in hours.  Used for expressing how much time is
 * available in particular RA bins.
 */
final case class Hours(hours: Double) {
    if (hours < 0) throw new IllegalArgumentException();
    @deprecated def getHours = hours
    def add(that: Hours): Hours = Hours(this.hours + that.hours)
    override def toString() = String.format("%.2f hrs", hours)
}

object Hours:
  val ZERO = Hours(0)
  def fromMillisec(ms: Long): Hours = Hours(ms / 3600000.0)

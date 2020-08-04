// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import gsp.math.JulianDate
import java.time.ZoneId
import java.time.Duration

object Constants {
  // some (not all) physical, mathematical, and astronomical constants used are defined here.
  val TwicePi = 6.28318530717959
  val HalfPi  = 1.57079632679490 // From Abramowitz & Stegun

  val ArcsecsInRadian = 206264.8062471
  val DegsInRadian    = 57.2957795130823
  val HrsInRadian     = 3.819718634205
  val KmSInAUDay      = 1731.45683633 // km per sec in 1 AU/day

  val SpeedOfLight = 299792.458 // in km per sec ... exact.

  val J2000 = JulianDate.J2000.toDouble

  val SecsInDay         = 86400.0
  val FlatteningOfEarth = 0.003352813 // FlatteningOfEarthing of earth, 1/298.257

  val EquatorialRadiusMeters = 6378137.0 // equatorial radius of earth, meters

  val MetersInAU = 1.4959787066e11 // 1 AU in meters

  val KZen =
    0.172 // zenith extinction, mag, for use in lunar sky brightness calculations.

  val EarthDiff =
    0.05 // used in numerical differentiation to find earth velocity

  val UT: ZoneId           = ZoneId.of("UT")
  val NanosPerSecond: Long = Duration.ofSeconds(1).toNanos
}

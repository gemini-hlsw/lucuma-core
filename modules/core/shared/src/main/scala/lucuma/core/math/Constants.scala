// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import java.time.Duration

object Constants {

  /*
   * Astronomical Constants
   */

  /** One AU in meters. */
  val AstronomicalUnit: Long = 149597870660L

  /** Meters/sec in 1 AU/day. */
  val MetersPerSecondInAUPerDay: Double = 1731456.83633

  /** Flattening of earth, 1/298.257 */
  val FlatteningOfEarth: Double = 0.003352813

  /** Equatorial radius of Earth in meters. */
  val EquatorialRadiusMeters: Int = 6378137

  /** 1 AU in meters. */
  val MetersInAU: Double = 1.4959787066e11

  /** Zenith extinction as magnitude. For use in lunar sky brightness calculations. */
  val KZen: Double = 0.172

  /** Used in numerical differentiation to find Earth velocity. */
  val EarthDiff: Double = 0.05

  /*
   * Physical Constants
   */

  /** Speed of light in meters per second. Exact. */
  val SpeedOfLight: Int = 299792458

  /** Seconds in a Day. For Convenience. */
  val SecsInDay: Long = Duration.ofDays(1).getSeconds

  /** Nanos in a Second. For Convenience. */
  val NanosInSecond: Long = Duration.ofSeconds(1).toNanos

  /*
   * Mathematical Constants
   */

  /** 2π, to higher precision than what you get in stdlib. */
  val TwoPi: Double = 6.283185307179586476925286766559

  /** π/2, from Abramowitz & Stegun. */
  val HalfPi: Double = 1.57079632679490

  /** Seconds in Radian. */
  val ArcsecsInRadian: Double = 206264.8062471

  /** Degrees in Radian. */
  val DegsInRadian: Double = 57.2957795130823

  /** Hours in Radian. */
  val HrsInRadian: Double = 3.819718634205

}

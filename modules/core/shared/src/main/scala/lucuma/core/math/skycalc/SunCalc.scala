// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats.syntax.all._
import lucuma.core.math.JulianDate
import lucuma.core.math.Constants._

trait SunCalc extends ImprovedSkyCalcMethods {
  protected def lst(jd: JulianDate, longit: Double): Double =
    lst(jd.toDouble, longit)

  /**
    * returns altitude(degr) for dec, ha, lat (decimal degr, hr, degr)
    *
    * @param dec target declination in degrees
    * @param ha  the hour angle in hours
    * @param lat the observer's latitude in degrees
    * @return the altitude in degrees
    */
  protected def altit(dec: Double, ha: Double, lat: Double): Double = {
    val az     = new DoubleRef()
    val parang = new DoubleRef()
    return altit(dec, ha, lat, az, parang)
  }

  /**
    * returns jd at which sun is at a given
    * altitude, given jdguess as a starting point. Uses
    * low-precision sun, which is plenty good enough.
    */
  protected def jd_sun_alt(
    alt:      Double,
    jdguess0: JulianDate,
    lat:      Double,
    longit:   Double
  ): Option[JulianDate] = {
    val del = 0.002

    var jdguess = jdguess0

    /* first guess */
    var sun = lpsun(jdguess)
    var ra  = sun._1
    var dec = sun._2

    val ha   = lst(jdguess, longit) - ra
    val alt2 = altit(dec, ha, lat)
    jdguess = JulianDate.fromDoubleApprox(jdguess.toDouble + del)

    sun = lpsun(jdguess)
    ra = sun._1
    dec = sun._2

    var alt3  = altit(dec, lst(jdguess, longit) - ra, lat)
    var err   = alt3 - alt
    val deriv = (alt3 - alt2) / del
    var i     = 0

    while ((Math.abs(err) > 0.1) && (i < 10)) {
      jdguess = JulianDate.fromDoubleApprox(jdguess.toDouble - err / deriv)
      sun = lpsun(jdguess)
      ra = sun._1
      dec = sun._2

      alt3 = altit(dec, lst(jdguess, longit) - ra, lat)
      err = alt3 - alt
      i += 1
    }
    // If None: sunrise, set, or twilight calculation not converging.
    jdguess.some.filter(_ => i < 9)
  }

  /**
    * Returns hour angle at which object at dec is at altitude alt.
    * If object is never at this altitude, signals with special
    * return values 1000 (always higher) and -1000 (always lower).
    */
  protected def ha_alt(dec0: Double, lat0: Double, alt: Double): Double = {
    var dec = dec0
    var lat = lat0

    var min_max: Array[Double] = null
    try min_max = min_max_alt(lat, dec)
    catch {
      case _: Exception =>
        return 1000.0
    }
    if (alt < min_max(0)) return 1000.0  // flag value - always higher than asked
    if (alt > min_max(1)) return -1000.0 // flag for object always lower than asked
    dec = HalfPi - dec / DegsInRadian
    lat = HalfPi - lat / DegsInRadian
    val coalt = HalfPi - alt / DegsInRadian
    val x     = (Math.cos(coalt) - Math.cos(dec) * Math.cos(lat)) / (Math.sin(dec) * Math.sin(lat))
    if (Math.abs(x) <= 1.0)
      Math.acos(x) * HrsInRadian
    else
      throw new RuntimeException("Error in ha_alt ... acos(>1).")
  }

  /**
    * Computes minimum and maximum altitude for a given dec and
    * latitude.
    *
    * @return 2 element double array where element 0 is the min altitude
    *         and element 1 is the max altitude
    */
  protected def min_max_alt(lat0: Double, dec0: Double): Array[Double] = {
    var lat = lat0
    var dec = dec0

    var min = 0.0
    var max = 0.0
    lat = lat / DegsInRadian
    dec = dec / DegsInRadian
    var x   = Math.cos(dec) * Math.cos(lat) + Math.sin(dec) * Math.sin(lat)
    if (Math.abs(x) <= 1.0) max = Math.asin(x) * DegsInRadian
    else
      throw new RuntimeException("Error in min_max_alt -- arcsin(>1)")
    x = Math.sin(dec) * Math.sin(lat) - Math.cos(dec) * Math.cos(lat)
    if (Math.abs(x) <= 1.0) min = Math.asin(x) * DegsInRadian
    else
      throw new RuntimeException("Error in min_max_alt -- arcsin(>1)")
    Array[Double](min, max)
  }

  /**
    * Low precision sun.
    *
    * @return Coordinates in (Hours, Degress)
    */
  protected def lpsun(jd: JulianDate): (Double, Double) = {
    val n       = jd.toDouble - JulianDate.J2000.toDouble
    val L       = 280.460 + 0.9856474 * n
    val g       = (357.528 + 0.9856003 * n) / DegsInRadian
    val lambda  = (L + 1.915 * Math.sin(g) + 0.020 * Math.sin(2.0 * g)) / DegsInRadian
    val epsilon = (23.439 - 0.0000004 * n) / DegsInRadian
    val x       = Math.cos(lambda)
    val y       = Math.cos(epsilon) * Math.sin(lambda)
    val z       = Math.sin(epsilon) * Math.sin(lambda)
    val ra      = atan_circ(x, y) * HrsInRadian
    val dec     = Math.asin(z) * DegsInRadian
    (ra, dec)
  }

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import Constants._

import cats.implicits._
import lucuma.core.math.JulianDate
import lucuma.core.math.Place
import java.time.Instant
import java.time.LocalDate

trait TwilightCalc extends SunCalc {

  /** Compute start and end of night for a particular date and place.
    *
    * The night will be bounded by twilight as defined by
    * [[lucuma.core.math.skycalc.TwilightBoundType]]. It will be the night that starts
    * on the given date and ends on the following day.
    *
    * @param boundType twilight bound type to use
    * @param date date when the night starts
    * @param place place on Earth
    *
    * @return A tuple of [[java.time.Instant]]s containing sunset and sunrise
    * respectively, or None if there's no sunset or sunrise for the
    * provided parameters.
    */
  def calculate(
    boundType: TwilightBoundType,
    date:      LocalDate,
    place:     Place
  ): Option[(Instant, Instant)] = {
    val nextMidnight = date.atStartOfDay(place.timezone).plusDays(1)
    val jdmid        = JulianDate.ofInstant(nextMidnight.toInstant)

    // Sunrise/set take altitude into account whereas the twilights don't.
    //
    // "The various twilights (6,12,18) describe how bright the sky is, and this does not depend on the altitude of
    // the observer, however, the time of sunrise and sunset does.  For example, consider Hilo and Maunakea.  The
    // sky brightness above them is the same, while the time when they see the sun dip below the horizon is not"
    // -- Andrew Stephens 2017-11-14

    val angle: Double = boundType match {
      case TwilightBoundType.Official =>
        // Horizon geometric correction from p. 24 of the Skycalc manual: sqrt(2 * elevation / Re) (radians)
        boundType.horizonAngle + Math.sqrt(
          2.0 * place.altitudeDouble / TwilightCalc.EQUAT_RAD
        ) * TwilightCalc.DEG_IN_RADIAN
      case _                          => boundType.horizonAngle
    }

    calcTimes(angle, jdmid, place)
  }

  private def calcTimes(
    angle: Double,
    jdmid: JulianDate,
    place: Place
  ): Option[(Instant, Instant)] = { // (Start, End)
    val (rasun, decsun) = lpsun(jdmid)

    val lat    = place.latitude.toAngle.toSignedDoubleDegrees
    val longit = -(place.longitude.toSignedDoubleDegrees / 15.0) // skycalc wants hours

    val hasunset = ha_alt(decsun, lat, -angle)
    if (hasunset > 900.0) // flag for never sets
      none                // Sun up all night
    else if (hasunset < -900.0)
      none                // Sun down all day
    else {
      val stmid = lst(jdmid, longit)
      // initial guess
      var tmp   = jdmid.toDouble + adj_time(rasun + hasunset - stmid) / 24.0
      // more accurate
      jd_sun_alt(-angle, JulianDate.fromDoubleApprox(tmp), lat, longit).flatMap { jdSet =>
        // initial guess
        tmp = jdmid.toDouble + adj_time(rasun - hasunset - stmid) / 24.0
        // more accurate
        jd_sun_alt(-angle, JulianDate.fromDoubleApprox(tmp), lat, longit).map { jdRise =>
          (jdSet.toInstant, jdRise.toInstant)
        }
      }
    }
  }
}

object TwilightCalc extends TwilightCalc

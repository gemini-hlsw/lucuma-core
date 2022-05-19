// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats.syntax.all._
import coulomb.refined._
import lucuma.core.`enum`.TwilightType
import lucuma.core.math.Constants._
import lucuma.core.math.JulianDate
import lucuma.core.math.Place
import lucuma.core.optics.Spire
import org.typelevel.cats.time._
import spire.math.Bounded
import spire.math.extras.interval.IntervalSeq
import spire.std.double._

import java.time.Instant
import java.time.LocalDate

trait TwilightCalc extends SunCalc {

  /**
   * Compute start and end of night for a particular date and place.
   *
   * The night will be bounded by twilight as defined by
   * [[lucuma.core.`enum`.TwilightType]]. It will be the night that starts
   * on the given date and ends on the following day.
   *
   * @param twilightType twilight bound type to use
   * @param date date when the night starts
   * @param place place on Earth
   *
   * @return A tuple of [[java.time.Instant]]s containing sunset and sunrise
   * respectively, or None if there's no sunset or sunrise for the
   * provided parameters.
   */
  def forDate(
    twilightType: TwilightType,
    date:         LocalDate,
    place:        Place
  ): Option[Bounded[Instant]] = {
    val nextMidnight = date.atStartOfDay(place.timezone).plusDays(1)
    val jdmid        = JulianDate.ofInstant(nextMidnight.toInstant)

    // Sunrise/set take altitude into account whereas the twilights don't.
    //
    // "The various twilights (6,12,18) describe how bright the sky is, and this does not depend on the altitude of
    // the observer, however, the time of sunrise and sunset does.  For example, consider Hilo and Maunakea.  The
    // sky brightness above them is the same, while the time when they see the sun dip below the horizon is not"
    // -- Andrew Stephens 2017-11-14

    val angle: Double = twilightType match {
      case TwilightType.Official =>
        // Horizon geometric correction from p. 24 of the Skycalc manual: sqrt(2 * elevation / Re) (radians)
        twilightType.horizonAngle.toAngle.toSignedDoubleDegrees + Math.sqrt(
          2.0 * (place.altitude.toValue[Double] / EquatorialRadius[Double]).value
        ) * DegsInRadian
      case _                     => twilightType.horizonAngle.toAngle.toSignedDoubleDegrees
    }

    calcTimes(angle, jdmid, place).flatMap(Spire.openUpperIntervalFromTuple[Instant].getOption)
  }

  def forBoundedInterval(
    twilightType: TwilightType,
    interval:     Bounded[Instant],
    place:        Place
  ): IntervalSeq[Instant] = {
    val (start, end)      = Spire.openUpperIntervalFromTuple[Instant].reverseGet(interval)
    val startDate         = start.atZone(place.timezone).toLocalDate
    val endDate           = end.atZone(place.timezone).toLocalDate
    val dates             =
      List.unfold(startDate)(date => if (date <= endDate) (date, date.plusDays(1)).some else none)
    val twilightIntervals = dates.flatMap(d => forDate(twilightType, d, place))
    Spire.intervalListUnion[Instant].get(twilightIntervals) & interval
  }

  private def calcTimes(
    angle: Double,
    jdmid: JulianDate,
    place: Place
  ): Option[(Instant, Instant)] = { // (sunset, sunrise)
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

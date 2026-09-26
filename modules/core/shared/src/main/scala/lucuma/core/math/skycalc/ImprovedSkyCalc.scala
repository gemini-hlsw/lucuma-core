// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import lucuma.core.enums.Site
import lucuma.core.math.Constants.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Place

import java.time.Instant
import java.time.ZonedDateTime

/**
 * Improved version of SkyCalc that supports lunar calculations. All instance stuff is here;
 * the trait is exclusively static stuff.
 * @author brighton, rnorris
 */
case class ImprovedSkyCalc(place: Place) extends ImprovedSkyCalcMethods {
  val degreesLatitude = place.latitude.toAngle.toSignedDoubleDegrees
  val hoursLongitude  = -place.longitude.toSignedDoubleDegrees / 15
  val siteAltitude    = place.altitudeDouble

  def calculate(
    coords:        Coordinates,
    instant:       Instant,
    calculateMoon: Boolean
  ): SkyCalcResults = {
    val jdut     = new DoubleRef
    val sid      = new DoubleRef
    val curepoch = new DoubleRef
    setup_time_place(instant, hoursLongitude, jdut, sid, curepoch)
    val objra    = coords.ra.toAngle.toSignedDoubleDegrees / 15
    val objdec   = coords.dec.toAngle.toSignedDoubleDegrees
    val objepoch = 2000.0
    getCircumstances(
      degreesLatitude,
      siteAltitude,
      objra,
      objdec,
      objepoch,
      curepoch.d,
      sid.d,
      degreesLatitude,
      jdut,
      calculateMoon,
      coords
    )
  }

  private def getCircumstances(
    degreesLatitude: Double,
    siteAltitude:    Double,
    objra:           Double,
    objdec:          Double,
    objepoch:        Double,
    curep:           Double,
    sid:             Double,
    lat:             Double,
    jdut:            DoubleRef,
    calculateMoon:   Boolean,
    coords:          Coordinates
  ): SkyCalcResults = {
    var lunarSkyBrightness: java.lang.Double = .0
    var lunarDistance                        = .0
    var lunarIlluminatedFraction             = .0
    var totalSkyBrightness                   = .0
    var lunarPhaseAngle                      = .0
    var sunAltitude                          = .0
    var lunarElevation                       = .0

    val az     = new DoubleRef
    val par    = new DoubleRef
    val curra  = new DoubleRef
    val curdec = new DoubleRef
    cooxform(
      objra,
      objdec,
      objepoch,
      curep,
      curra,
      curdec,
      XFORM_JUSTPRE,
      XFORM_FROMSTD
    )
    val ha = adj_time(sid - curra.d)
    val alt = altit(curdec.d, ha, lat, az, par)
    val airmass = getAirmass(alt)
    val altitude = alt
    val azimuth = az.d
    val parallacticAngle = par.d
    val hourAngle = ha
    if (calculateMoon) {
      val ramoon      = new DoubleRef
      val decmoon     = new DoubleRef
      val distmoon    = new DoubleRef
      val georamoon   = new DoubleRef
      val geodecmoon  = new DoubleRef
      val geodistmoon = new DoubleRef
      val rasun       = new DoubleRef
      val decsun      = new DoubleRef
      val distsun     = new DoubleRef
      val x           = new DoubleRef
      val y           = new DoubleRef
      val z           = new DoubleRef
      val toporasun   = new DoubleRef
      val topodecsun  = new DoubleRef
      val elevsea     = siteAltitude
      accusun(
        jdut.d,
        sid,
        degreesLatitude,
        rasun,
        decsun,
        distsun,
        toporasun,
        topodecsun,
        x,
        y,
        z
      )
      sunAltitude = altit(
        topodecsun.d,
        sid - toporasun.d,
        degreesLatitude,
        az,
        new DoubleRef /* [out] parang, ignored */
      )
      accumoon(
        jdut.d,
        degreesLatitude,
        sid,
        elevsea,
        georamoon,
        geodecmoon,
        geodistmoon,
        ramoon,
        decmoon,
        distmoon
      )
      lunarElevation = altit(decmoon.d, sid - ramoon.d, degreesLatitude, az, new DoubleRef)
      // Sky brightness
      lunarSkyBrightness = null
      lunarDistance = DegsInRadian * subtend(ramoon.d, decmoon.d, objra, objdec)
      lunarPhaseAngle = DegsInRadian * subtend(ramoon.d, decmoon.d, toporasun.d, topodecsun.d)
      if (lunarElevation > -2.0)
        if ((lunarElevation > 0.0) && (altitude > 0.5) && (sunAltitude < -9.0))
          lunarSkyBrightness = lunskybright(
            lunarPhaseAngle,
            lunarDistance,
            KZen,
            lunarElevation,
            altitude,
            distmoon.d
          )
      totalSkyBrightness = sb(
        180.0 - lunarPhaseAngle,
        lunarDistance,
        90.0 - lunarElevation,
        90.0 - altitude,
        90.0 - sunAltitude,
        distmoon.d
      )
      lunarIlluminatedFraction = (0.5 * (1.0 - Math.cos(
        subtend(ramoon.d, decmoon.d, rasun.d, decsun.d)
      )))
    }

    SkyCalcResults(
      altitude,
      azimuth,
      parallacticAngle,
      airmass,
      hourAngle,
      lunarIlluminatedFraction.toFloat,
      lunarSkyBrightness,
      totalSkyBrightness,
      lunarPhaseAngle,
      sunAltitude,
      lunarDistance,
      lunarElevation,
      coords,
      place
    )
  }

  /**
   * Return the sidereal time for the given instant at the given site, in decimal hours.
   */
  def getSiderealTime(instant: Instant): Double = {
    val jd = instant_to_jd(instant)
    lst(jd, hoursLongitude)
  }

  /**
   * Return the LST time for the given instant at the given site.
   */
  def getLst(instant: Instant): ZonedDateTime = {
    val lstHours = getSiderealTime(instant)
    getLst(lstHours, instant)
  }

  /**
   * Return the next meridian transit (upper culmination) of the given sidereal coordinates,
   * strictly after the given instant. Precision is to the millisecond.
   */
  def nextTransit(coords: Coordinates, after: Instant): Instant = {
    val hourAngleAtAfter: Double = hourAngleHours(coords, after)
    // Normalised to [0, 24) so that a target transiting exactly at `after` maps to the next transit.
    val normalisedHourAngle: Double =
      if (hourAngleAtAfter < 0.0) hourAngleAtAfter + 24.0 else hourAngleAtAfter
    val estimate: Instant           =
      after.plusMillis(siderealHoursToSolarMillis(24.0 - normalisedHourAngle))
    val refined: Instant            = refineTransit(coords, estimate)
    // Refinement may land on or before `after` when the transit is within rounding of it.
    if (refined.isAfter(after)) refined
    else refineTransit(coords, estimate.plusMillis(siderealHoursToSolarMillis(24.0)))
  }

  // Uses the hour angle against the RA precessed to the current epoch, consistent with `calculate`.
  private def hourAngleHours(coords: Coordinates, instant: Instant): Double =
    calculate(coords, instant, false).hourAngleRaw

  // One correction pass absorbs the drift between the mean sidereal rate and precession.
  private def refineTransit(coords: Coordinates, estimate: Instant): Instant =
    estimate.minusMillis(siderealHoursToSolarMillis(hourAngleHours(coords, estimate)))

  private def siderealHoursToSolarMillis(siderealHours: Double): Long =
    Math.round(siderealHours / SiderealRate * 3600.0 * 1000.0)
}

object ImprovedSkyCalc {

  /**
   * Return the next meridian transit (upper culmination) of the given sidereal coordinates at the
   * given site, strictly after the given instant.
   */
  def nextTransit(site: Site, coords: Coordinates, after: Instant): Instant =
    ImprovedSkyCalc(site.place).nextTransit(coords, after)
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import lucuma.core.math.Coordinates
import lucuma.core.math.Place
import lucuma.core.math.Constants._
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
    var altitude                             = .0
    var hourAngle                            = .0
    var azimuth                              = .0
    var parallacticAngle                     = .0
    var airmass                              = .0
    var lunarSkyBrightness: java.lang.Double = .0
    var lunarDistance                        = .0
    var lunarIlluminatedFraction             = .0
    var totalSkyBrightness                   = .0
    var lunarPhaseAngle                      = .0
    var sunAltitude                          = .0
    var lunarElevation                       = .0

    var ha     = .0
    var alt    = .0
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
    ha = adj_time(sid - curra.d)
    alt = altit(curdec.d, ha, lat, az, par)
    airmass = getAirmass(alt)
    altitude = alt
    azimuth = az.d
    parallacticAngle = par.d
    hourAngle = ha
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
      lunarDistance =
        DegsInRadian * subtend(ramoon.d, decmoon.d, objra, objdec)
      lunarPhaseAngle =
        DegsInRadian * subtend(ramoon.d, decmoon.d, toporasun.d, topodecsun.d)
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
        90.0 - sunAltitude
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
    * Return the LST time for the given instant at the given site.
    */
  def getLst(instant: Instant): ZonedDateTime = {
    val jd       = instant_to_jd(instant)
    val lstHours = lst(jd, hoursLongitude)
    getLst(lstHours, instant)
  }
}

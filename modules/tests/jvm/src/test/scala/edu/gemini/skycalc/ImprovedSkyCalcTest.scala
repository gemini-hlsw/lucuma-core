// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.skycalc

import java.time.Instant
import java.time.ZonedDateTime

import edu.gemini.skycalc.ImprovedSkyCalcMethods._
import java.{ util => ju }
import jsky.coords.WorldCoords

/**
  * This class exists purely for testing purposes.
  * It provides the same functionality as {lucuma.core.math.skycalc.ImprovedSkyCalc},
  * but it extends the Java implementation of ImprovedSkyCalcMethods, so that
  * its results can be compared against its Scala version.
  *
  * Must be in edu.gemini.skycalc package to be able to access ImprovedSkyCalcMethods
  * protected members.
  */
final class ImprovedSkyCalcTest(
  val degreesLatitude: Double,
  longitude:           Double,
  val siteAltitude:    Double
) extends ImprovedSkyCalcMethods {

  // Site parameters
  private val hoursLongitude = -longitude / 15.0

  // calculated results
  private var altitude                             = .0
  private var hourAngle                            = .0
  private var azimuth                              = .0
  private var parallacticAngle                     = .0
  private var airmass                              = .0
  private var lunarSkyBrightness: java.lang.Double = .0
  private var lunarDistance                        = .0
  private var lunarIlluminatedFraction             = .0
  private var totalSkyBrightness                   = .0
  private var lunarPhaseAngle                      = .0
  private var sunAltitude                          = .0
  private var lunarElevation                       = .0

  def calculate(
    obj:           WorldCoords,
    date:          ju.Date,
    calculateMoon: Boolean
  ): Unit = {
    val dateTime = new DateTime(date)
    val jdut     = new DoubleRef
    val sid      = new DoubleRef
    val curepoch = new DoubleRef
    setup_time_place(dateTime, hoursLongitude, jdut, sid, curepoch)
    val objra    = obj.getRaDeg() / 15
    val objdec   = obj.getDecDeg()
    val objepoch = 2000.0
    getCircumstances(
      objra,
      objdec,
      objepoch,
      curepoch.d,
      sid.d,
      degreesLatitude,
      jdut,
      calculateMoon
    )
  }

  private def getCircumstances(
    objra:         Double,
    objdec:        Double,
    objepoch:      Double,
    curep:         Double,
    sid:           Double,
    lat:           Double,
    jdut:          DoubleRef,
    calculateMoon: Boolean
  ): Unit = {
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
    airmass = ImprovedSkyCalcMethods.getAirmass(alt)
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
        DEG_IN_RADIAN * subtend(ramoon.d, decmoon.d, objra, objdec)
      lunarPhaseAngle =
        DEG_IN_RADIAN * subtend(ramoon.d, decmoon.d, toporasun.d, topodecsun.d)
      if (lunarElevation > -2.0)
        if ((lunarElevation > 0.0) && (altitude > 0.5) && (sunAltitude < -9.0))
          lunarSkyBrightness = lunskybright(
            lunarPhaseAngle,
            lunarDistance,
            KZEN,
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
  }

  /**
    * Return the LST time for the given instant at the given site.
    */
  def getLst(instant: Instant): ZonedDateTime = {
    val date     = ju.Date.from(instant)
    val dateTime = new DateTime(date)
    val jd       = date_to_jd(dateTime)
    val lstHours = lst(jd, hoursLongitude)
    ZonedDateTime.ofInstant(ImprovedSkyCalcMethods.getLst(lstHours, date).toInstant(), UTC.toZoneId)
  }

  def getAltitude: Double = altitude

  def getAzimuth: Double = azimuth

  def getParallacticAngle: Double = parallacticAngle

  def getAirmass: Double = airmass

  def getHourAngle: Double = hourAngle

  def getLunarIlluminatedFraction: Float = lunarIlluminatedFraction.toFloat

  def getLunarSkyBrightness: Double = lunarSkyBrightness

  def getTotalSkyBrightness: Double = totalSkyBrightness

  def getLunarPhaseAngle: Double = lunarPhaseAngle

  def getSunAltitude: Double = sunAltitude

  def getLunarDistance: Double = lunarDistance

  def getLunarElevation: Double = lunarElevation
}

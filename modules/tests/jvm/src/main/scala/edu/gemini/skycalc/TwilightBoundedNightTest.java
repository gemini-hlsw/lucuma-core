/*
 * Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.skycalc;

import jsky.util.DateUtil;

import java.util.TimeZone;
import java.util.logging.Logger;
import java.util.logging.Level;

import java.util.Calendar;
import lucuma.math.Place;

/**
  * This class exists purely for testing purposes.
  * It provides the same functionality as {lucuma.math.skycalc.TwilightCalc},
  * but using the Java implementation of ImprovedSkyCalcMethods, so that
  * its results can be compared against its Scala version.
  *
  * Must be in edu.gemini.skycalc package to be able to access ImprovedSkyCalcMethods
  * protected members.
  */
public final class TwilightBoundedNightTest {
    private static final Logger LOG = Logger.getLogger(TwilightBoundedNightTest.class.getName());

    /**
     * Creates a {@link Night} bounded by twilight for the specified date.
     * It will be the night that starts on that date and ends on the following
     * day.
     *
     * @param type definition of twilight to use (how far below the horizon in
     * the west that the sun must be before night starts, and how far below
     * in the east before night ends)
     *
     * @param date day of month (where the first day is 1)
     * @param month month (use the Calendar constants -- in other words
     * January is 0 not 1)
     * @param year year of interest (AD)
     *
     * @param place the place in the world
     *
     * @return TwilightBoundedNightTest starting on the specified date
     */
    public static TwilightBoundedNightTest forDate(lucuma.math.skycalc.TwilightBoundType type, int date, int month, int year, Place place) {

        Calendar c = Calendar.getInstance(TimeZone.getTimeZone(place.zoneId()));

        // Set to midnight on that date
        c.set(Calendar.YEAR, year);
        c.set(Calendar.MONTH, month);
        c.set(Calendar.DATE, date);
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MILLISECOND, 0);

        return new TwilightBoundedNightTest(type, c.getTimeInMillis(), place);
    }

    /**
     * Creates a {@link Night} bounded by twilight, depending upon the
     * specified definition of twilight.
     *
     * @param type definition of twilight to use (how far below the horizon in
     * the west that that the sun must be before night starts, and how far
     * below in the east before night ends)
     *
     * @param time time of interest; if during a night local time, the night
     * returned will be that same night, if during the day local time, it
     * will be the night that starts on that day and ends on the following
     * day
     *
     * @param place the place in the world
     */
    public static TwilightBoundedNightTest forTime(lucuma.math.skycalc.TwilightBoundType type, long time, Place place) {
        TwilightBoundedNightTest tonight;
        tonight = new TwilightBoundedNightTest(type, time, place);

        // If we're in the time from the dusk (inclusive) to midnight
        // (exclusive), return the current night
        if (tonight.includes(time)) return tonight;

        // A night sits between two days.  It starts on one day and ends on the
        // next day.  TwilightBoundedNight will use the time to get a date,
        // for example October 9.  So from midnight local time on the 9th of
        // October to 11:59:59 PM the night it constructs is the night of
        // October 9/10.

        // So it is possible that the current time is during the night from
        // October 8/9. In other words, the time is between midnight and dawn.
        // So construct "yesterday night" and check.
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone(place.zoneId()));
        cal.setTimeInMillis(time);
        cal.add(Calendar.DAY_OF_YEAR, -1);

        long timeYesterday = cal.getTimeInMillis();
        TwilightBoundedNightTest yesterdayNight;
        yesterdayNight = new TwilightBoundedNightTest(type, timeYesterday, place);
        if (yesterdayNight.includes(time)) return yesterdayNight;

        // Okay, the current time is during the day so return the night that
        // is coming.
        return tonight;
    }

    private lucuma.math.skycalc.TwilightBoundType _type;
    private Place _place;
    private long _start;
    private long _end;

    /**
     * Creates a {@link Night} bounded by twilight, depending upon the
     * specified definition of twilight.
     *
     * @param type definition of twilight to use (how far below the horizon in
     * the west that that the sun must be before night starts, and how far
     * below in the east before night ends)
     *
     * @param time time of interest, which is only used to obtain the day;
     * midnight and 11:59:59 PM work will both yield the same result--a Night
     * configured for the night beginning on the date contained in
     * <code>time</code> in the timezone specified in <code>desc</code>
     *
     * @param place the place in the world
     */
    public TwilightBoundedNightTest(lucuma.math.skycalc.TwilightBoundType type, long time, Place place) {
        _type = type;
        _place = place;

        Calendar c = Calendar.getInstance(TimeZone.getTimeZone(place.zoneId()));
        c.setTimeInMillis(time);

        // Set to midnight.
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        c.set(Calendar.MILLISECOND, 0);
        c.add(Calendar.DAY_OF_YEAR, 1);

        // Get sunset.
        final JulianDate jdmid = new JulianDate(c.getTimeInMillis());

        // Sunrise/set take altitude into account whereas the twilights don't.
        //
        // "The various twilights (6,12,18) describe how bright the sky is, and this does not depend on the altitude of
        // the observer, however, the time of sunrise and sunset does.  For example, consider Hilo and Maunakea.  The
        // sky brightness above them is the same, while the time when they see the sun dip below the horizon is not"
        // -- Andrew Stephens 2017-11-14

        final double angle;
        if(type == lucuma.math.skycalc.TwilightBoundType.Official$.MODULE$)
            // Horizon geometric correction from p. 24 of the Skycalc manual: sqrt(2 * elevation / Re) (radians)

            angle = type.horizonAngle() + Math.sqrt(2.0 * place.altitudeDouble() / ImprovedSkyCalcMethods.EQUAT_RAD) *
                    ImprovedSkyCalcMethods.DEG_IN_RADIAN;
        else angle = type.horizonAngle();
        _calcTimes(angle, jdmid, place);
    }

    private void _calcTimes(double angle, JulianDate jdmid, Place place) {
        Coordinates sun = ImprovedSkyCalcMethods.lpsun(jdmid);
        double rasun  = sun.getRaDeg();
        double decsun = sun.getDecDeg();

        double lat    =   place.latitude().toAngle().toSignedDoubleDegrees();
        double longit = -(place.longitude().toSignedDoubleDegrees() / 15.0); // skycalc wants hours

        double hasunset = ImprovedSkyCalcMethods.ha_alt(decsun, lat, -angle);

        if (hasunset > 900.) {  // flag for never sets
            LOG.log(Level.WARNING, "Sun up all night on: " + jdmid.toDate());
            return;
        }

        if (hasunset < -900.) {
            LOG.log(Level.WARNING, "Sun down all day on: " + jdmid.toDate());
            return;
        }

        double stmid = ImprovedSkyCalcMethods.lst(jdmid, longit);

        // initial guess
        double tmp = jdmid.toDouble() + ImprovedSkyCalcMethods.adj_time(rasun + hasunset-stmid)/24.;
        JulianDate jdset = new JulianDate(tmp);

        // more accurate
        jdset = ImprovedSkyCalcMethods.jd_sun_alt(-angle, jdset, lat, longit);
        if (jdset == null) {
            LOG.log(Level.WARNING, "Sun doesn't set on: " + jdmid.toDate());
            return;
        }

        _start = jdset.toTimestamp();

        // initial guess
        tmp = jdmid.toDouble() + ImprovedSkyCalcMethods.adj_time(rasun - hasunset-stmid)/24.;
        JulianDate jdrise = new JulianDate(tmp);

        // more accurate
        jdrise = ImprovedSkyCalcMethods.jd_sun_alt(-angle, jdrise, lat, longit);
        if (jdrise == null) {
            LOG.log(Level.WARNING, "Sun doesn't rise on: " + jdmid.toDate());
            return;
        }

        _end = jdrise.toTimestamp();
    }

    public lucuma.math.skycalc.TwilightBoundType getType() {
        return _type;
    }

    public Place getPlace() {
        return _place;
    }

    public long getStartTime() {
        return _start;
    }

    public long getEndTime() {
        return _end;
    }

    /** Start time rounded to the nearest minute in the given timezone. */
    public long getStartTimeRounded(TimeZone zone) {
        return DateUtil.roundToNearestMinute(_start, zone);
    }

    /** End time rounded to the nearest minute in the given timezone. */
    public long getEndTimeRounded(TimeZone zone) {
        return DateUtil.roundToNearestMinute(_end, zone);
    }

    public long getTotalTime() {
        return _end - _start;
    }

    public boolean includes(long time) {
        return (_start <= time) && (time < _end);
    }
}

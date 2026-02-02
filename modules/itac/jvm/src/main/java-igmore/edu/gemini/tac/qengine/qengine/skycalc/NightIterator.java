/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.Night;

import edu.gemini.skycalc.TwilightBoundType;
import edu.gemini.skycalc.TwilightBoundedNight;

import edu.gemini.skycalc.Interval;
import edu.gemini.skycalc.Union;
import edu.gemini.spModel.core.Semester;
import edu.gemini.spModel.core.Site;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * An iterator over twilight bounded nights between two dates.
 */
public final class NightIterator implements Iterator<Night> {
    private static final TwilightBoundType DEFAULT = TwilightBoundType.NAUTICAL;

    private Night cur;
    private final long end;
    private final TwilightBoundType type;
    private final Site siteDesc;

    /**
     * An iterator over all the nights in the semester using nautical twilight
     * bounds.
     *
     * @param site determines the site to which the night times apply
     * @param semester determines the semester whose nights are iterated
     */
    public NightIterator(Site site, Semester semester) {
        this(site, semester, DEFAULT);
    }

    /**
     * An iterator over all the nights in the semester.
     *
     * @param site determines the site to which the night times apply
     * @param semester determines the semester whose nights are iterated
     * @param type definition of twilight
     */
    public NightIterator(Site site, Semester semester, TwilightBoundType type) {
        this(site, semester.getStartDate(site), semester.getEndDate(site), type);
    }

    /**
     * An iterator over all the nights that fall within the given start and end
     * dates using nautical twilight bounds.
     *
     * @param site determines the site to which the night times apply
     * @param start start date from which iteration begins
     * @param end end date at which iteration stops
     */
    public NightIterator(Site site, Date start, Date end) {
        this(site, start, end, DEFAULT);
    }

    // /**
    //  * An iterator over all the nights that fall within the given start and end
    //  * dates.
    //  *
    //  * @param site determines the site to which the night times apply
    //  * @param start start date from which iteration begins
    //  * @param end end date at which iteration stops
    //  * @param type definition of twilight
    //  */
    // public NightIterator(Site site, Date start, Date end, TwilightBoundType type) {
    //     this(SiteLookup.get(site), start, end, type);
    // }

    // public NightIterator(Site site, Date start, Date end) {
    //     this(site, start, end, DEFAULT);
    // }

    /**
     * An iterator over all the nights that fall within the given start and end
     * dates using nautical twilight bounds.
     *
     * @param site determines the site to which the night times apply
     * @param start start date from which iteration begins
     * @param end end date at which iteration stops
     */
    public NightIterator(Site site, Date start, Date end, TwilightBoundType type) {
        this.end      = end.getTime();
        this.type     = type;
        this.siteDesc = site;

        cur = firstNight(site, start.getTime(), end.getTime(), type);
    }

    // Determines the bounds of the first night boxed by at most the given
    // start and end times.
    private static Night firstNight(Site site, long s, long e, TwilightBoundType type) {
        if (e <= s) return null;

        // Adjust s and e to the start and end of the first night.
        Night night = new TwilightBoundedNight(type, s, site);
        long start  = Math.max(night.getStartTime(), s);
        long end    = Math.min(night.getEndTime(), e);

        return (end <= start) ? null : new NightInterval(site, start, end);
    }

    // Calculates roughly the next night start time. It will be off by seconds
    // but that's okay because the TwilightBoundedNight will figure out the
    // real start time for the night.
    private long nextNightStartTime() {
        Calendar cal = new GregorianCalendar(siteDesc.timezone());
        cal.setTimeInMillis(cur.getStartTime());
        cal.add(Calendar.DAY_OF_MONTH, 1);
        return cal.getTimeInMillis();
    }

    private Night nextNight() {
        Night night = new TwilightBoundedNight(type, nextNightStartTime(), siteDesc);
        if (night.getEndTime() > end) {
            if (night.getStartTime() >= end) {
                night = null;
            } else {
                night = new NightInterval(siteDesc, night.getStartTime(), end);
            }
        }
        return night;
    }

    public boolean hasNext() { return cur != null; }


    public Night next() {
        Night night = cur;
        cur = nextNight();
        return night;
    }

    public void remove() { throw new UnsupportedOperationException(); }

    /**
     * Creates a list of all the nights produced by the iterator.
     */
    public List<Night> toList() {
        if (!hasNext()) return Collections.emptyList();
        List<Night> res = new ArrayList<Night>();
        while (hasNext()) res.add(next());
        return res;
    }
}

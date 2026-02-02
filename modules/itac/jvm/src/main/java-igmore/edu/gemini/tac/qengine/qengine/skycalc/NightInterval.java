/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.Night;

import edu.gemini.skycalc.TwilightBoundType;
import edu.gemini.skycalc.TwilightBoundedNight;
import edu.gemini.skycalc.Interval;
import edu.gemini.skycalc.Union;
import edu.gemini.spModel.core.Semester;
import edu.gemini.spModel.core.Site;

import java.util.*;

/**
 * A NightInterval is an Interval that can work as a Night as well.  This class
 * implements the Night interface in terms of an Interval.  Provides methods
 * that split up an interval into any twilight bounded nights they contain.
 */
public class NightInterval extends Interval implements Night {

    private final edu.gemini.spModel.core.Site site;

    public NightInterval(edu.gemini.spModel.core.Site site, long start, long end) {
        super(start, end);
        this.site = site;
    }

    @Override
    protected NightInterval create(long start, long end) {
        return new NightInterval(site, start, end);
    }

    @Override
    public edu.gemini.spModel.core.Site getSite() { return site; }

    @Override
    public long getStartTime() { return getStart(); }

    @Override
    public long getEndTime() { return getEnd(); }

    @Override
    public long getTotalTime() { return getLength(); }

    @Override
    public boolean includes(long l) { return contains(l); }
}

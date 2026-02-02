/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.spModel.core.Semester;
import edu.gemini.spModel.core.Site;

import edu.gemini.skycalc.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

/**
 * Calculates RaBin times based upon the method extracted from the
 * "RAdistributions.xls" spreadsheet used in the ITAC process prior to the
 * introduction of the automatted system.
 */
public final class ExcelRaBinCalc implements RaBinCalc {
    private static final TwilightBoundType DEFAULT_BOUNDS = TwilightBoundType.NAUTICAL;

    private final TwilightBoundType bounds;

    /**
     * Constructs with nautical twilight bounds.
     */
    public ExcelRaBinCalc() {
        this(DEFAULT_BOUNDS);
    }

    /**
     * @param bounds defines how twilight boundaries for evening and morning
     * are set
     */
    public ExcelRaBinCalc(TwilightBoundType bounds) {
        this.bounds = bounds;
    }

    @Override
    public List<Hours> calc(Site site, Date start, Date end, RaBinSize size) {
        int binCount = size.getBinCount();

        long[] totals   = new long[binCount];
        long binSizeMs  = size.getSize() * 60 * 1000; // convert from min to ms
        long halfSizeMs = size.getSize() * 30 * 1000;

        // Skycalc lst algorithm expects a west longitude (hence -1 is
        // multiplied) expressed in hours, not degrees (hence, divide by 15).
        double longit = -1 * site.longitude / 15.0;

        List<Angle> ras = size.genRas();

        Iterator<Night> it = new NightIterator(site, start, end, bounds);
        while (it.hasNext()) {
            Night night = it.next();

            long startTime = night.getStartTime();
            long endTime   = night.getEndTime();

            JulianDate jdStart = new JulianDate(startTime);
            JulianDate jdEnd   = new JulianDate(endTime);

            double eve  = ImprovedSkyCalc.lst(jdStart, longit);
            double morn = ImprovedSkyCalc.lst(jdEnd,   longit);

            // Temporary patch until https://github.com/gemini-hlsw/ocs/pull/2051 propagates out.
            // It's harmless thereafter so not a big deal.
            if (eve < 0)  eve += 24;
            if (morn < 0) morn += 24;

            for (int bin=0; bin<size.getBinCount(); ++bin) {
                double ra = ras.get(bin).toHours().getMagnitude();
                if (eve < morn) {
                    if (ra>eve && ra<morn) totals[bin] += binSizeMs;
                } else if (morn<ra) {
                    if (eve<ra) totals[bin] += binSizeMs;
                } else {
                    if (morn>ra) totals[bin] += binSizeMs;
                }
            }
        }

        List<Hours> res = new ArrayList<Hours>(binCount);
        for (int i=0; i<binCount; ++i) res.add(Hours.fromMillisec(totals[i]));
        return res;
    }

    public static void main(String[] args) throws Exception {
        RaBinSize    sz = new RaBinSize(3 * 60);
        Site       site = Site.GS;
        Semester    sem = Semester.parse("2020A");
        Date      start = sem.getStartDate(site);
        Date        end = sem.getEndDate(site);
        List<Hours> hrs = (new ExcelRaBinCalc()).calc(site, start, end, sz);

        int i = 0;
        for (Hours h : hrs) {
            System.out.println(i + " " + h.getHours());
            i++;
        }

    }
}

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

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Calculated RA bin times and Dec bin percentages.
 */
public final class RaDecBinCalc {

    // Using the Excel method since that most closely tracks what they expect.
    public static final RaBinCalc RA_CALC   = new ExcelRaBinCalc();

    // Using elevation method since I don't have another way.
    public static final DecBinCalc DEC_CALC = new ElevationDecBinCalc();

    private final List<Hours> raHours;
    private final List<Percent> decPercents;

    private RaDecBinCalc(List<Hours> raHours, List<Percent> decPercents) {
        this.raHours     = raHours;
        this.decPercents = decPercents;
    }

    public List<Hours> getRaHours() { return raHours; }
    public List<Percent> getDecPercentages() { return decPercents; }

    private static class RaAndHourPair {
        final Hours hrs;
        final Angle ra;

        RaAndHourPair(Hours hrs, Angle ra) {
            this.hrs = hrs;
            this.ra  = ra;
        }
    }

    private static RaAndHourPair max(RaBinSize raSize, List<Hours> hrs) {
        List<Angle> ras = raSize.genRas();
        Hours max = hrs.get(0);
        Angle ra  = raSize.genRas().get(0);
        for (int i=1; i<raSize.getBinCount(); ++i) {
            Hours cur = hrs.get(i);
            if (cur.getHours() > max.getHours()) {
                max = cur;
                ra  = ras.get(i);
            }
        }
        return new RaAndHourPair(max, ra);
    }

    /**
     * Calculates the RA/Dec bin and returns the result.
     *
     * @param site Gemini site to which the calculation corresponds
     * @param semester semester in which the calculation is valid
     * @param raSize size of the ra bins in minutes
     * @param decSize size of the dec bins in degrees
     *
     * @return RA/Dec bin calculation
     */
    public static RaDecBinCalc calc(Site site, Date start, Date end, RaBinSize raSize, DecBinSize decSize) {
        // Calculate the times to reserve for each RA.
        List<Hours> raHours = RA_CALC.calc(site, start, end, raSize);

        // Get the RA and time amount of the most visible RA.
        RaAndHourPair p = max(raSize, raHours);

        // Use it to calculate dec percentages.
        List<Percent> decPercents = DEC_CALC.calc(site, start, end, decSize, p.ra);

        return new RaDecBinCalc(raHours, decPercents);
    }

    private static final class Key {
        final Site site;
        final Semester semester;
        final RaBinSize raBinSize;
        final DecBinSize decBinSize;

        Key(Site site, Semester semester, RaBinSize raBinSize, DecBinSize decBinSize) {
            if ((site == null) || (semester == null) || (raBinSize == null) || (decBinSize == null)) {
                throw new IllegalArgumentException("cannot construct with null");
            }

            this.site       = site;
            this.semester   = semester;
            this.raBinSize  = raBinSize;
            this.decBinSize = decBinSize;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Key key = (Key) o;

            if (!decBinSize.equals(key.decBinSize)) return false;
            if (!raBinSize.equals(key.raBinSize)) return false;
            if (!semester.equals(key.semester)) return false;
            if (site != key.site) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = site.hashCode();
            result = 31 * result + semester.hashCode();
            result = 31 * result + raBinSize.hashCode();
            result = 31 * result + decBinSize.hashCode();
            return result;
        }
    }

    private static final int    CACHE_SIZE = 50;
    private static final float LOAD_FACTOR =  0.75f;

    private static int cacheCapacity() {
        return (int) Math.round(Math.ceil((CACHE_SIZE + 1) / LOAD_FACTOR));
    }

    private static final Map<Key, RaDecBinCalc> CACHE = new LinkedHashMap<Key, RaDecBinCalc>(cacheCapacity(), LOAD_FACTOR, true) {
        @Override public boolean removeEldestEntry(Map.Entry<Key, RaDecBinCalc> me) {
            return size() > CACHE_SIZE;
        }
    };

    /**
     * Obtains the RA/Dec bin calculation corresponding to the given site,
     * semester, and RA/Dec bin sizes.  If the value has been calculated
     * previously and is still cached, it returns immediately with the result.
     * Otherwise, it performs the calculation and caches the answer.
     *
     * @param site Gemini site to which the calculation corresponds
     * @param semester semester in which the calculation is valid
     * @param raSize size of the ra bins in minutes
     * @param decSize size of the dec bins in degrees
     *
     * @return RA/Dec bin calculation
     */
    public static RaDecBinCalc get(Site site, Semester semester, RaBinSize raSize, DecBinSize decSize) {
        Key key = new Key(site, semester, raSize, decSize);

        RaDecBinCalc calc;
        synchronized (CACHE) { calc = CACHE.get(key); }

        if (calc == null) {
            Date start = semester.getStartDate(site);
            Date end   = semester.getEndDate(site);
            calc = calc(site, start, end, raSize, decSize);
            synchronized (CACHE) { CACHE.put(key, calc); }
        }

        return calc;
    }
}

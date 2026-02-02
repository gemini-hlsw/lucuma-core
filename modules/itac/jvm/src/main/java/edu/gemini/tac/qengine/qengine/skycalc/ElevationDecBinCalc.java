/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.Angle;
import edu.gemini.skycalc.Night;

import edu.gemini.skycalc.ElevationConstraintSolver;
import edu.gemini.skycalc.Interval;
import edu.gemini.skycalc.Solver;
import edu.gemini.skycalc.Union;
import edu.gemini.spModel.core.Site;
import jsky.coords.WorldCoords;

import java.util.*;

/**
 * Calculates dec bin percentages based upon airmass elevation constraints.
 */
public class ElevationDecBinCalc implements DecBinCalc {
    private final ElevationConfig conf;

    /**
     * Calculates DecBin percentages based upon the default configuration values.
     */
    public ElevationDecBinCalc() {
        this(ElevationConfig.DEFAULT);
    }

    /**
     * @param conf definition of when to consider a target observable
     */
    public ElevationDecBinCalc(ElevationConfig conf) {
        this.conf = conf;
    }

    public ElevationConfig getConfig() { return conf; }

    private List<Solver> getSolvers(Site site, List<WorldCoords> targets) {
        List<Solver> res = new ArrayList<Solver>();

        // targets.map(t => ElevationConstraintSolver.forAirmass(site, t, minAirmass, maxAirmass))
        for (WorldCoords wc : targets) {
            res.add(ElevationConstraintSolver.forAirmass(site, wc, conf.getMinAirmass(), conf.getMaxAirmass()));
        }
        return res;
    }

    // Gets the index of the bin that corresponds to the site.  All percentage
    // calculations are relative to this bin.
    private static int getSiteIndex(edu.gemini.spModel.core.Site site, DecBinSize size) {
        return ((int) Math.floor(site.latitude + 90.0)) / size.getSize();
    }

    @Override
    public List<Percent> calc(Site site, Date start, Date end, DecBinSize size, Angle ra) {
        List<WorldCoords> targets = size.genTargets(ra);
        List<Solver>      solvers = getSolvers(site, targets);

        long[] totals = new long[size.getBinCount()];

        Iterator<Night> it = new NightIterator(site, start, end, conf.getBounds());
        while (it.hasNext()) {
            int bin = 0;
            Night night = it.next();
            long startTime = night.getStartTime();
            long endTime   = night.getEndTime();
            for (Solver s : solvers) {
                Union<Interval> u = s.solve(startTime, endTime);
                for (Interval i : u.getIntervals()) {
                    totals[bin] += i.getLength();
                }
                ++bin;
            }
        }

        // Take as the max the value that corresponds to the dec overhead for
        // the site.
        long max = totals[getSiteIndex(site, size)];
//        for (long cur : totals) max = (cur > max) ? cur : max;

        List<Percent> res = new ArrayList<Percent>(size.getBinCount());
        for (long cur : totals) {
            res.add(new Percent(100.0 * ((double) cur / max)));
        }
        return res;
    }
}

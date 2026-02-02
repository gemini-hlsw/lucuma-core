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
import edu.gemini.skycalc.Angle;
import edu.gemini.skycalc.Night;

import edu.gemini.skycalc.ElevationConstraintSolver;
import edu.gemini.skycalc.Interval;
import edu.gemini.skycalc.Solver;
import edu.gemini.skycalc.Union;
import jsky.coords.WorldCoords;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

/**
 * Calculates hours per bin based upon airmass elevation constraints.
 */
public final class ElevationRaBinCalc implements RaBinCalc {

    private final boolean transitOnly;
    private final ElevationConfig conf;

    /**
     * Calculates RaBin times based upon the default configuration values:
     * <ul>
     * <li>transitOnly - true</li>
     * <li>bounds - nautical</li>
     * <li>minAirmass - 1.0 (overhead at zenith)</li>
     * <li>maxAirmass - 2.0</li>
     * </ul>
     */
    public ElevationRaBinCalc() {
        this(true, ElevationConfig.DEFAULT);
    }

    /**
     * @param transitOnly only count up to RaBinSize minutes maximum each night
     * of the semester that meets the elevation constraints
     * @param conf definition of when to consider a target observable
     */
    public ElevationRaBinCalc(boolean transitOnly, ElevationConfig conf) {
        this.transitOnly = transitOnly;
        this.conf        = conf;
    }

    public ElevationConfig getConfig() { return conf; }

    private static List<WorldCoords> getTargets(edu.gemini.spModel.core.Site site, RaBinSize size) {
        List<Angle> lst = size.genRas();
        double dec = site.latitude;  // Use a target overhead for the site

        // lst.map(a => new WorldCoords(a.toDegrees.getMagnitude, dec))
        List<WorldCoords> res = new ArrayList<WorldCoords>();
        for (Angle ra : lst) {
            res.add(new WorldCoords(ra.toDegrees().getMagnitude(), dec));
        }
        return res;
    }

    private List<Solver> getSolvers(edu.gemini.spModel.core.Site site, RaBinSize size) {
        List<WorldCoords> targets = getTargets(site, size);
        List<Solver> res = new ArrayList<Solver>();

        // targets.map(t => ElevationConstraintSolver.forAirmass(site, t, minAirmass, maxAirmass))
        for (WorldCoords wc : targets) {
            res.add(ElevationConstraintSolver.forAirmass(site, wc, conf.getMinAirmass(), conf.getMaxAirmass()));
        }
        return res;
    }

    @Override
    public List<Hours> calc(Site site, Date start, Date end, RaBinSize size) {
        int binCount = size.getBinCount();

        long[] totals  = new long[binCount];
        long binSizeMs = size.getSize() * 60 * 1000; // ms

        List<Solver> solvers = getSolvers(site, size);

        Iterator<Night> it = new NightIterator(site, start, end, conf.getBounds());
        while (it.hasNext()) {
            int bin = 0;
            Night night = it.next();
            long startTime = night.getStartTime();
            long endTime   = night.getEndTime();
            for (Solver s : solvers) {
                Union<Interval> u = s.solve(startTime, endTime);
                for (Interval i : u.getIntervals()) {
                    long ms      = i.getLength();
                    totals[bin] += transitOnly ? Math.min(binSizeMs, ms) : ms;
                }
                ++bin;
            }
        }

        List<Hours> res = new ArrayList<Hours>(binCount);
        for (int i=0; i<binCount; ++i) res.add(Hours.fromMillisec(totals[i]));
        return res;
    }
}

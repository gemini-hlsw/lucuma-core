/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.Angle;
import jsky.coords.WorldCoords;

import java.util.ArrayList;
import java.util.List;

/**
 * Dec bin size which is expressed in degrees.
 */
public final class DecBinSize extends BinSize {
    /** Default dec bin size of 10 degrees. */
    public static final DecBinSize DEFAULT;

    public static final int DEFAULT_DEGREES = 10;

    static {
        try {
            DEFAULT = new DecBinSize(DEFAULT_DEGREES);
        } catch (BinSizeException ex) {
            throw new RuntimeException(ex);
        }
    }

    public DecBinSize(int deg) throws BinSizeException {
        super(BinSize.Type.DEC, deg);
    }
    public BinSize.Type getType() { return BinSize.Type.DEC; }

    /**
     * Generates a list of Angle, one per bin, expressing the location of the
     * center of the bin.
     */
    public List<Angle> genDecs() { return genAngles(-90); }

    /**
     * Gets a list of WorldCoords targets for the middle of the dec bins at the
     * given RA.
     */
    public List<WorldCoords> genTargets(Angle ra) {
        double raDeg = ra.toDegrees().getMagnitude();

        List<Angle> decs = genDecs();
        List<WorldCoords> res = new ArrayList<WorldCoords>(decs.size());
        for (Angle dec : decs) {
            res.add(new WorldCoords(raDeg, dec.toDegrees().getMagnitude()));
        }

        return res;
    }
}

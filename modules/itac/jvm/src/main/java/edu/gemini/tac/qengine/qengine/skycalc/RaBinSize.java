/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.Angle;

import java.util.List;

/**
 * RA bin size, which is expressed in minutes
 * (1 minute = 15/60 degrees = 0.25 degrees). A 1 hour RA bin would therefore
 * be expressed as <code>new RaBinSize(60)</code>.
 */
public final class RaBinSize extends BinSize {
    /** Default RA bin size of 1 hour (60 minutes). */
    public static final RaBinSize DEFAULT;
    static {
        try {
            DEFAULT = new RaBinSize(60);  // 1 hour
        } catch (BinSizeException ex) {
            throw new RuntimeException(ex);
        }
    }

    public RaBinSize(int min) throws BinSizeException {
        super(BinSize.Type.RA, min);
    }

    public BinSize.Type getType() { return BinSize.Type.RA; }

    /**
     * Generates a list of Angle, one per bin, expressing the location of the
     * center of the bin.
     */
    public List<Angle> genRas() { return genAngles(0); }
}

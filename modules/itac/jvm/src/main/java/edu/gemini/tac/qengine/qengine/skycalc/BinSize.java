/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.Angle;

import java.util.ArrayList;
import java.util.List;

/**
 * Contract implemented by RA and Dec bin sizes.
 */
public abstract class BinSize {
    public static final int TOTAL_RA_MIN  =  24 * 60;  // 24 hours * 60 min
    public static final int TOTAL_DEC_DEG = 180 ;      // 180 deg

    enum Type {
        RA() {
            public int getTotal() { return TOTAL_RA_MIN; }
            public String getName() { return "ra"; }
            public Angle.Unit getUnits() { return Angle.Unit.MINUTES; }
        },
        DEC() {
            public int getTotal() { return TOTAL_DEC_DEG; }
            public String getName() { return "dec"; }
            public Angle.Unit getUnits() { return Angle.Unit.DEGREES; }
        },
        ;

        /** Gets the total number of units in the valid range. */
        public abstract int getTotal();

        /** Gets the name of the bin size type. */
        public abstract String getName();

        /** Gets the name of the units in which the bin size is measured. */
        public abstract Angle.Unit getUnits();
    }

    private final int size;

    protected BinSize(Type type, int size) throws BinSizeException {
        BinSizeException.validate(type, size);
        this.size = size;
    }

    /** Get the bin size type. */
    public abstract Type getType();


    /** Gets the bin size. */
    public int getSize() { return size; }

    /** Gets the bin count for this size. */
    public int getBinCount() { return getType().getTotal() / size; }

    protected List<Angle> genAngles(double offset) {
        List<Angle> res = new ArrayList<Angle>();
        double halfBinSize = getSize() / 2.0;
        for (int i=0; i<getBinCount(); ++i) {
            res.add(new Angle(i*getSize() + halfBinSize + offset, getType().getUnits()));
        }
        return res;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        BinSize binSize = (BinSize) o;
        if (size != binSize.size) return false;
        if (getType() != binSize.getType()) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = getType().hashCode();
        result = 31 * result + size;
        return result;
    }
}

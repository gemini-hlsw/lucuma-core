/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

/**
 * Contains a percentage amount.  Used for expressing how much time to discount
 * from the total possible for declinations towards the north and south
 * horizons.
 */
public final class Percent {
    private final double perc;

    public Percent(double amount) {
        if (amount < 0) throw new IllegalArgumentException();
        this.perc = amount;
    }

    /** Gets percentage amount. */
    public double getAmount() { return perc; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Percent percent = (Percent) o;

        if (Double.compare(percent.perc, perc) != 0) return false;

        return true;
    }

    @Override
    public int hashCode() {
        long temp = perc != +0.0d ? Double.doubleToLongBits(perc) : 0L;
        return (int) (temp ^ (temp >>> 32));
    }

    @Override
    public String toString() { return String.format("%2.3f%%", perc); }

}

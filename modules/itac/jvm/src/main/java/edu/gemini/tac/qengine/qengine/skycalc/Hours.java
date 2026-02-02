/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

/**
 * Contains a time amount in hours.  Used for expressing how much time is
 * available in particular RA bins.
 */
public final class Hours {
    public static final Hours ZERO = new Hours(0);

    private final double hours;

    public Hours(double hours) {
        if (hours < 0) throw new IllegalArgumentException();
        this.hours = hours;
    }

    /** Total hours visible. */
    public double getHours() { return hours; }

    public Hours add(Hours that) { return new Hours(this.hours + that.hours); }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Hours that = (Hours) o;
        if (Double.compare(that.hours, hours) != 0) return false;

        return true;
    }

    @Override
    public int hashCode() {
        long temp = hours != +0.0d ? Double.doubleToLongBits(hours) : 0L;
        return (int) (temp ^ (temp >>> 32));
    }

    @Override
    public String toString() { return String.format("%.2f hrs", hours); }

    public static Hours fromMillisec(long ms) {
        return new Hours(ms / 3600000.0);
    }
}

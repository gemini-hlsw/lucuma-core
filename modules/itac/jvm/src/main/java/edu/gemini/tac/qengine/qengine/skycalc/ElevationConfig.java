/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import edu.gemini.skycalc.TwilightBoundType;

/**
 * Configuration for elevation-based RA/Dec bin calculations.
 */
public final class ElevationConfig {
    public static final TwilightBoundType DEFAULT_BOUNDS = TwilightBoundType.NAUTICAL;
    public static final double DEFAULT_MIN_AIRMASS = 1.00;
    public static final double DEFAULT_MAX_AIRMASS = 2.15;

    /**
     * A default elevation config with the following properties.
     * <ul>
     * <li>bounds - nautical</li>
     * <li>minAirmass - 1.0 (overhead at zenith)</li>
     * <li>maxAirmass - 2.15</li>
     * </ul>
     */
    public static final ElevationConfig DEFAULT =
        new ElevationConfig(DEFAULT_BOUNDS, DEFAULT_MIN_AIRMASS, DEFAULT_MAX_AIRMASS);

    private final TwilightBoundType bounds;
    private final double minAirmass;
    private final double maxAirmass;


    /**
     * @param bounds how the twilight boundaries are defined
     * @param minAirmass minimum airmass value (thinest airmass)
     * @param maxAirmass maximum airmass value (thickest airmass)
     */
    public ElevationConfig(TwilightBoundType bounds, double minAirmass, double maxAirmass) {
        this.bounds      = bounds;
        this.minAirmass  = minAirmass;
        this.maxAirmass  = maxAirmass;
    }

    public TwilightBoundType getBounds() { return bounds; }
    public double getMinAirmass() { return minAirmass; }
    public double getMaxAirmass() { return maxAirmass; }
}

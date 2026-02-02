/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

/**
 * Exception that indicates an attempt to create a BinSize with an invalid
 * size.
 */
public final class BinSizeException extends Exception {

    private static final String ODD_SIZE_MESSAGE = "Bad bin size: %d %s. %s bin size must evenly divide %d %s.";
    private static final String formatOddSize(BinSize.Type type, int size) {
        return String.format(ODD_SIZE_MESSAGE, size, type.getUnits(), type.getName(), type.getTotal(), type.getUnits());
    }

    private static final String NEGATIVE_SIZE_MESSAGE = "Bad bin size: %d %s. Cannot be negative.";
    private static final String formatNegativeSize(BinSize.Type type, int size) {
        return String.format(NEGATIVE_SIZE_MESSAGE, size, type.getUnits());
    }

    private final int badSize;
    private BinSizeException(String msg, int size) {
        super(msg);
        this.badSize = size;
    }

    public int getBadSize() { return badSize; }

    public static void validate(BinSize.Type type, int size) throws BinSizeException {
        if ((size == 0) || (type.getTotal() % size != 0)) {
            throw new BinSizeException(formatOddSize(type, size), size);
        }
        if (size < 0) {
            throw new BinSizeException(formatNegativeSize(type, size), size);
        }
        // okay
    }
}

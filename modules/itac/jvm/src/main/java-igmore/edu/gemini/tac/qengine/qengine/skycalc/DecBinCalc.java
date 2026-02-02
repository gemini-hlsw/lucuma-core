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

import java.util.Date;
import java.util.List;

/**
 * Describes the contract for calculating the DecBin percentages.
 */
public interface DecBinCalc {

    List<Percent> calc(Site site, Date start, Date end, DecBinSize size, Angle ra);
}

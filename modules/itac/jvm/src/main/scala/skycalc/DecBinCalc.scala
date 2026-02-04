// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc;

import edu.gemini.qengine.skycalc.DecBinSize
import edu.gemini.tac.qengine.util.Percent
import lucuma.core.enums.Site
import lucuma.core.math.RightAscension

import java.time.Instant

trait DecBinCalc:
  def calc(site: Site, start: Instant, end: Instant, size: DecBinSize, ra: RightAscension): List[Percent] 
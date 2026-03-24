// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine

import edu.gemini.tac.qengine.p1.ItacTarget
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import munit.FunSuite

class ItacSuite extends FunSuite:
  
  extension (self: ItacTarget.type)
    def apply(ra: Double, dec: Double): ItacTarget =
      ItacTarget(
        Coordinates(
          RightAscension(HourAngle.fromDoubleDegrees(ra)), 
          Declination.fromDoubleDegrees(dec).get
        ), 
        lucuma.core.model.Target.Id.fromLong(1).get
      )


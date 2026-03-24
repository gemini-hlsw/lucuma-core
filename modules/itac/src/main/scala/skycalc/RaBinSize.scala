// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension

sealed abstract case class RaBinSize(arcMinutes: Int):

  def binCount: Int = 1440 / arcMinutes

  def genRas: List[RightAscension] =
    val half = arcMinutes.toDouble / 2.0
    (0 until binCount).toList.map: b =>
      RightAscension(HourAngle.fromDoubleMinutes(b * arcMinutes + half))

object RaBinSize:

  def ofArcMinutes(arcMinutes: Int): Option[RaBinSize] =
    Some(arcMinutes)
      .filter(_ > 0)
      .filter(1440 % _ == 0)
      .map(new RaBinSize(_) {})

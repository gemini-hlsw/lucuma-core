// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import edu.gemini.tac.qengine.util.Angle

object Target {
  def apply(raDeg: Double, decDeg: Double, name: String): Target =
    new Target(new Angle(raDeg, Angle.Deg), new Angle(decDeg, Angle.Deg), Option(name))

  def apply(raDeg: Double, decDeg: Double): Target =
    new Target(new Angle(raDeg, Angle.Deg), new Angle(decDeg, Angle.Deg))
}

case class Target(ra: Angle, dec: Angle, name: Option[String] = None)

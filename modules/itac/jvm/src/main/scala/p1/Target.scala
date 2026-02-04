// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.math.Coordinates
import lucuma.core.math.RightAscension
import lucuma.core.math.HourAngle
import lucuma.core.math.Declination

case class Target(coords: Coordinates, name: Option[String] = None):
  def ra = coords.ra
  def dec = coords.dec

object Target:
  // TODO: deprecate
  def apply(ra: Double, dec: Double): Target =
    Target(Coordinates(RightAscension(HourAngle.fromDoubleDegrees(ra)), Declination.fromDoubleDegrees(dec).get))
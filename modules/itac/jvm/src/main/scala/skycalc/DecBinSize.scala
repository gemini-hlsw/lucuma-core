// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import lucuma.core.math.Declination
import lucuma.core.math.Coordinates
import lucuma.core.math.RightAscension

sealed abstract case class DecBinSize(degrees: Int):
  
  def binCount: Int = 180 / degrees

  def genDecs: List[Declination] =
    val half = degrees.toDouble / 2.0
    (0 until binCount).toList.map: b =>
      Declination.fromDoubleDegrees(b * degrees + half - 90).get

  def genCoordinates(ra: RightAscension): List[Coordinates] =
    genDecs.map(Coordinates(ra, _))

object DecBinSize:

  def ofDegrees(degrees: Int): Option[DecBinSize] =
    Some(degrees)
      .filter(_ > 0)
      .filter(180 % _ == 0)
      .map(new DecBinSize(_) {})

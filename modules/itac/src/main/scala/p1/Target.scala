// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import lucuma.core.math.Coordinates

case class Target(coords: Coordinates, name: Option[String]):
  def ra = coords.ra
  def dec = coords.dec
